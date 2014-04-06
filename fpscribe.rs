#![feature(phase)]
#[phase(syntax, link)] extern crate log;

use std::io;
use std::io::{IoResult, IoError};


fn cap_main(args: || -> ~[~str], usb_hids: ~Rd) {
    let dev_num = args()[1];
    match usb_hids.sub_rd(dev_num) {
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
    events: Receiver<~[PedalEvent]>
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

    fn new(device: ~Rd: Send) -> FootPedal {
        let (tx, rx) = channel();

        spawn(proc() {
            let mut stream = device.open_rd();
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
    fn unpack(data: ~[u8]) -> ~[PedalEvent] {
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
trait Rd: Send {
    fn full_path(&self) -> ~str;
    fn sub_rd(&self, n: &str) -> IoResult<~Rd: Send>;
    fn open_rd(&self) -> IoResult<~Reader>;
}


struct HidDevs {
    dev_usb: ~Rd: Send
}

impl HidDevs {
    fn new(fs: ~Rd: Send) -> IoResult<HidDevs> {
        fs.sub_rd("/dev/usb").map( |rd| HidDevs { dev_usb: rd } )
    }
}

impl Rd for HidDevs {
    fn sub_rd(&self, n: &str) -> IoResult<~Rd: Send> {
        if n.char_len() == 1 && n.char_at(0).is_digit() {
            self.dev_usb.sub_rd("hiddev" + n)
        } else {
            Err(IoError{ desc: "not a device number",
                         detail: Some(n.to_owned()),
                         kind: io::PermissionDenied })
        }
    }

    fn full_path(&self) -> ~str {
        self.dev_usb.full_path()
    }

    fn open_rd(&self) -> IoResult<~Reader> {
        Err(IoError{ desc: "cannot open directory",
                     detail: None,
                     kind: io::MismatchedFileTypeForOperation })
    }
}


struct ArgvRd {
    argv: ~[~str],
    rd: ~Rd: Send
}

impl Rd for ArgvRd {
    fn sub_rd(&self, n: &str) -> IoResult<~Rd: Send> {
        if self.argv.iter().any(|s| s.as_slice() == n) {
            self.rd.sub_rd(n)
                } else {
            Err(IoError{ desc: "not CLI arg",
                         detail: Some(n.to_owned()),
                         kind: io::PermissionDenied })
        }
    }

    fn full_path(&self) -> ~str {
        self.rd.full_path()
    }

    fn open_rd(&self) -> IoResult<~Reader> {
        self.rd.open_rd()
    }
}


mod trusted {
    use std::io::IoResult;
    use std::io::fs::File;

    pub struct PosixFile {
        pub path: ~str
    }

    impl ::Rd for PosixFile {
        fn full_path(&self) -> ~str {
            self.path.clone()
        }

        fn sub_rd(&self, n: &str) -> IoResult<~::Rd: Send> {
            let sub_path: ~str = self.path + "/" + n; // TODO: real path join
            Ok(~PosixFile{ path: sub_path} as ~::Rd:Send)
        }

        fn open_rd(&self) -> IoResult<~Reader> {
            let rf: IoResult<File> = File::open(&Path::new(self.path.clone()));
            rf.map(|f| ~f as ~Reader)
        }
    }
}

fn main() {
    use std::os::args;

    let root_rd: ~Rd:Send = ~::trusted::PosixFile{ path: ~"/" };
    let hid_devs = ~HidDevs::new(root_rd).unwrap() as ~Rd:Send;
    let arg_dev_rd = ~ArgvRd{ argv: args(), rd: hid_devs } as ~Rd;

    cap_main(args, arg_dev_rd)
}
