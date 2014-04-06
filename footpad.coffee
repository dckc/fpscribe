# footpad -- treat footpedal as gamepad

endpoint = new WebSocket('ws://127.0.0.1:8080')
endpoint.onopen = -> console.log 'endpoint connection open'

endpoint.onmessage = (msg) ->
        e = JSON.parse msg.data
        console.log('event: ' + e)
