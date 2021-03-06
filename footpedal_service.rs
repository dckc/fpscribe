#![feature(phase)]
#[phase(syntax, link)] extern crate log;

extern crate footpedal;

use std::io::IoResult;
use std::io::fs::File;

use footpedal::{Rd, HidDevs, ArgvRd, FootPedal};


pub fn cap_main(args: || -> ~[~str], usb_hids: ~Rd) {
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


struct PosixFile {
    path: ~str
}

impl Rd for PosixFile {
    fn full_path(&self) -> ~str {
        self.path.clone()
    }
    
    fn sub_rd(&self, n: &str) -> IoResult<~Rd: Send> {
        let sub_path: ~str = self.path + "/" + n; // TODO: real path join
        Ok(~PosixFile{ path: sub_path } as ~Rd:Send)
    }
    
    fn open_rd(&self) -> IoResult<~Reader> {
        let rf: IoResult<File> = File::open(&Path::new(self.path.clone()));
        rf.map(|f| ~f as ~Reader)
    }
}


fn main() {
    use std::os::args;

    let root_rd: ~Rd:Send = ~PosixFile{ path: ~"/" };
    let hid_devs = ~HidDevs::new(root_rd).unwrap() as ~Rd:Send;
    let arg_dev_rd = ~ArgvRd{ argv: args(), rd: hid_devs } as ~Rd;

    cap_main(args, arg_dev_rd)
}
