#![feature(phase)]
#[phase(plugin, link)] extern crate log;

use std::io;
use std::io::{IoResult, IoError, Reader};


fn cap_main(args: || -> Vec<String>, usb_hids: Box<Rd>) {
    let dev_num = args()[1].clone();
    match usb_hids.sub_rd(dev_num.as_slice()) {
        Err(why) => fail!(format!("{}", why)),
        Ok(hid) => {
            let fp: FootPedal = FootPedal::new(hid);
            loop {
                let e = fp.events.recv();
                debug!("event: {}", e)
            }
        }
    }
}


struct FootPedal {
    events: Receiver<Vec<PedalEvent>>
}


static pedal_ixs: [int, ..3] = [1, 2, 3];
static pedal_data_size: uint = 8;
static event_size: uint = pedal_data_size * 3;

impl FootPedal {
    //! VEC USB Footpedal (USB device idVendor=05f3, idProduct=00ff)
    //! registers as hid device.
    //!
    //! ack: [footpedal]() explains that each pedal action generates three
    //! groups of 8 characters.
    //!
    //! [footpedal]: https://code.google.com/p/footpedal/

    fn new(device: Box<Rd+Send>) -> FootPedal {
        let (tx, rx) = channel();

        spawn(proc() {
            let mut stream = device.open_rd().unwrap();
            loop {
                match stream.read_exact(event_size) {
                    Ok(data) => tx.send(FootPedal::unpack(data)),
                    Err(why) => fail!("{}", why)
                }
            }
        });

        FootPedal{ events: rx }
    }

    /** Unpack footpedal event data.

    >>> chunks = [chr(ix) + '\x00\x00\x00\x01\x00\x00\x00'
    ...           for ix in range(1, 4)]
    >>> data = ''.join(chunks)
    >>> FootPedal.unpack(data)
    ... # doctest: +NORMALIZE_WHITESPACE
    [PedalEvent(ix=1, pressed=1),
     PedalEvent(ix=2, pressed=1),
     PedalEvent(ix=3, pressed=1)]
    */
    fn unpack(data: Vec<u8>) -> Vec<PedalEvent> {
        let each = |n: &int| {
            let ix = (n - 1) as uint;
            PedalEvent{ ix: data[ix * pedal_data_size],
                        pressed: data[ix * pedal_data_size + 4] }
        };
        pedal_ixs.iter().map(each).collect()
    }
}


#[deriving(Show)]
struct PedalEvent {
    ix: u8,
    pressed: u8  // TODO: bool
}


// Read access rights.
trait Rd {
    fn full_path(&self) -> String;
    fn sub_rd(&self, n: &str) -> IoResult<Box<Rd+Send>>;
    fn open_rd(&self) -> IoResult<Box<Reader+Send>>;
}


struct HidDevs {
    dev_usb: Box<Rd+Send>
}

impl HidDevs {
    fn new(fs: Box<Rd>) -> IoResult<HidDevs> {
        fs.sub_rd("/dev/usb").map( |rd| HidDevs { dev_usb: rd } )
    }
}

impl Rd for HidDevs {
    fn sub_rd(&self, n: &str) -> IoResult<Box<Rd+Send>> {
        if n.char_len() == 1 && n.char_at(0).is_digit() {
            self.dev_usb.sub_rd(("hiddev".to_string() + n.to_string()).as_slice())
        } else {
            Err(IoError{ desc: "not a device number",
                         detail: Some(n.to_string()),
                         kind: io::PermissionDenied })
        }
    }

    fn full_path(&self) -> String {
        self.dev_usb.full_path()
    }

    fn open_rd(&self) -> IoResult<Box<Reader+Send>> {
        Err(IoError{ desc: "cannot open directory",
                     detail: None,
                     kind: io::MismatchedFileTypeForOperation })
    }
}


struct ArgvRd {
    argv: Vec<String>,
    rd: Box<Rd+Send>
}

impl Rd for ArgvRd {
    fn sub_rd(&self, n: &str) -> IoResult<Box<Rd+Send>> {
        if self.argv.iter().any(|s| s.as_slice() == n) {
            self.rd.sub_rd(n)
                } else {
            Err(IoError{ desc: "not CLI arg",
                         detail: Some(n.to_string()),
                         kind: io::PermissionDenied })
        }
    }

    fn full_path(&self) -> String {
        self.rd.full_path()
    }

    fn open_rd(&self) -> IoResult<Box<Reader+Send>> {
        self.rd.open_rd()
    }
}


mod trusted {
    use std::io::IoResult;
    use std::io::fs::File;

    use super::Rd;

    pub struct PosixFile {
        pub path: String
    }

    impl ::Rd for PosixFile {
        fn full_path(&self) -> String {
            self.path.clone()
        }

        fn sub_rd(&self, n: &str) -> IoResult<Box<Rd+Send>> {
            let sub_path = self.path + "/" + n; // TODO: real path join
            Ok(box PosixFile{ path: sub_path} as Box<Rd+Send>)
        }

        fn open_rd(&self) -> IoResult<Box<Reader+Send>> {
            let rf: IoResult<File> = File::open(&Path::new(self.path.clone()));
            rf.map(|f| box f as Box<Reader+Send>)
        }
    }
}

fn main() {
    use std::os::args;

    let root_rd: Box<Rd> = box ::trusted::PosixFile{ path: "/".to_string() };
    let hid_devs = box HidDevs::new(root_rd).unwrap() as Box<Rd+Send>;
    let arg_dev_rd = box ArgvRd{ argv: args(), rd: hid_devs } as Box<Rd>;

    cap_main(args, arg_dev_rd)
}
