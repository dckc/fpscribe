
service = (dev) ->
        dev.on('data',
                (chunk) -> console.log(decode(chunk)))

decode = (chunk) ->
         each = (ix) -> 
            pedal: chunk[ix * 8]
            pressed: chunk[ix * 8 + 4]
          (each(ix) for ix in [0, 1, 2])

with_caps = () ->
        fs = require 'fs'
        dev = fs.createReadStream('/dev/usb/hiddev0')
        service(dev)

with_caps()
