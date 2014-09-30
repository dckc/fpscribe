
service = (dev, endpoint) ->
        console.log 'awaiting connection...'
        endpoint.on 'connection', (conn) ->
                console.log 'got connection!'
                console.log 'awaiting foot pedal event...'
                dev.on 'data',
                       (chunk) ->
                        event = decode chunk
                        console.log event
                        conn.send JSON.stringify(event)


decode = (chunk) ->
         each = (ix) -> 
            pedal: chunk[ix * 8]
            pressed: chunk[ix * 8 + 4]
          (each(ix) for ix in [0, 1, 2])

with_caps = () ->
        fs = require 'fs'
        ws = require 'ws'

        endpoint = new ws.Server {port: 8080}
        dev = fs.createReadStream '/dev/usb/hiddev0'
        service dev, endpoint

with_caps()
