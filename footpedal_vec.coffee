fs = require 'fs'

service = () ->
        dev = fs.createReadStream('/dev/usb/hiddev0')
        dev.on('data', (chunk) -> console.log('chunk: ' + chunk))


service()
