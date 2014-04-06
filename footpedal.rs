//#![no_std]
#![crate_type = "lib"]

#[no_implicit_prelude]

use std::io::{IoResult,
              IoError,
              PermissionDenied,
              MismatchedFileTypeForOperation};


pub struct FootPedal {
    pub events: Receiver<~[PedalEvent]>,
    pub errs: Receiver<IoError>
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

    pub fn new(device: ~Rd: Send) -> FootPedal {
        let (tx, rx) = channel();
        let (tx_err, rx_err) = channel();

        spawn(proc() {
            let mut stream = device.open_rd();
            loop {
                match stream.read_exact(event_size) {
                    Ok(data) => tx.send(FootPedal::unpack(data)),
                    Err(why) => tx_err.send(why)
                }
            }
        });

        FootPedal{ events: rx, errs: rx_err }
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


#[deriving(Show, Eq)]
pub struct PedalEvent {
    ix: u8,
    pressed: u8  // TODO: bool
}


// Read access rights.
pub trait Rd: Send {
    fn full_path(&self) -> ~str;
    fn sub_rd(&self, n: &str) -> IoResult<~Rd: Send>;
    fn open_rd(&self) -> IoResult<~Reader>;
}


pub struct HidDevs {
    dev_usb: ~Rd: Send
}


impl HidDevs {
    pub fn new(fs: ~Rd: Send) -> IoResult<HidDevs> {
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
                         kind: PermissionDenied })
        }
    }

    fn full_path(&self) -> ~str {
        self.dev_usb.full_path()
    }

    fn open_rd(&self) -> IoResult<~Reader> {
        Err(IoError{ desc: "cannot open directory",
                     detail: None,
                     kind: MismatchedFileTypeForOperation })
    }
}


pub struct ArgvRd {
    pub argv: ~[~str],
    pub rd: ~Rd: Send
}

impl Rd for ArgvRd {

    fn sub_rd(&self, n: &str) -> IoResult<~Rd: Send> {
        use std::io::PermissionDenied;

        if self.argv.iter().any(|s| s.as_slice() == n) {
            self.rd.sub_rd(n)
                } else {
            Err(IoError{ desc: "not CLI arg",
                         detail: Some(n.to_owned()),
                         kind: PermissionDenied })
        }
    }

    fn full_path(&self) -> ~str {
        self.rd.full_path()
    }

    fn open_rd(&self) -> IoResult<~Reader> {
        self.rd.open_rd()
    }
}
