# fpscribe -- foot pedal transcription

On a MacBook, we use [express scribe] with a VEC USB footpedal.
Could a Chromebox do likewise?
How good is the Web platform support?
We get the media via dropbox, after all.

 [express scribe]: http://www.nch.com.au/scribe/

## Foot pedals and the HTML5 Gamepad API

First clue:

> USB foot pedals tend to register as joysticks. Check /dev/input/js0
> -- [foot pedal for medical transcription][1]

But how to get at it from Javascript? Ah...

> [gamepad.js] is a Javascript library to enable using gamepads and
> joysticks in the browser.

I plugged it in and looked at the linux kernel messages; it registers
as hid device. (USB device idVendor=05f3, idProduct=00ff). But the
gamepad API didn't detect it.

 [1]: http://www.linuxquestions.org/questions/suse-novell-60/foot-pedal-for-medical-transcription-366401/
 [gamepad.js]: http://www.gamepadjs.com/

## A footpedal Websockets service

I found python support for the device in [footpedal]; it shows that
explains that each pedal action generates three groups of 8 characters
on `/dev/usb/hiddev0`.

`fpscribe.py` decodes and logs these events; it weighs in at around
100 lines, including [capability security idioms][2].

 [footpedal]: https://code.google.com/p/footpedal/
 [2]: http://www.madmode.com/2013/python-capability-idioms-part-1.html

### The rust IO API is coming along

The rust 0.10 release announcement prompted me to review the docs
again, and it looked like they had found time to flesh out the
standard library quite bit. The ppa of nightlies makes it trivial to
install now. I was able to try it out myself by porting `fpscribe.py`
to `fpscribe.rs`. I'm happy they went back from conditions to sum
types (enums) for IO error handling.

I was able to separate the unprivileged code as its own library crate,
but when I tried to tame the standard library, I ran into a
[compiler bug].

 [compiler bug]: https://github.com/mozilla/rust/issues/13364

### Capsicum on linux still too bleeding edge

It would have taken 500GB to set up a virtual machine (user-mode linux
style).

Bummer... I was interested to try to mix in the L4 API and/or genode.

### node.js is stabilizing

This is the first time I've picked up node.js where I didn't find my
installation from a few months ago horribly out of date. 0.10.26 has
been the latest stable since February.

Getting the basic "log the pedal events" functionality going was a
simple matter of familiarizing myself with the [fs] module.

I thought about trying purescript or fantasyland promises but just
went with [coffeescript], though it adds a build step without adding
much early detection of errors. I should probably move on from using a
`Makefile` and learn the JS build tools; is `grunt` the
state-of-the-art? I seem to see it in various places
(e.g. fantasyland).

The server side of WebSockets is entirely straightforward using [ws].

 [fs]: http://nodejs.org/api/fs.html
 [coffeescript]: http://coffeescript.org/
 [ws]: https://www.npmjs.org/package/ws

In just a few minutes with the [websockets tutorial][3], I had the client side working in Chrome.

 [3]: http://www.html5rocks.com/en/tutorials/websockets/basics/
