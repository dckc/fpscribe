# footpad -- treat footpedal as gamepad

FootPad = (index, mk_socket, clock, handler) ->
        this.id = 'VEC USB footpedal 05f3:00ff'
        this.index = index
        this.connected = false
        this.timestamp = clock()
        this.mapping = 'footpedal'  # hmm...
        this.axes = []

        self = this
        a_button = () ->
                pressed: false
                value: 0.0
                update: (state) ->
                        this.pressed = state.pressed
                        this.value = state.pressed ? 1.0 : 0.0

        this.buttons = (a_button() for ix in [0, 1, 2])

        pedal_remote = mk_socket()
        pedal_remote.onopen = ->
                console.log 'footpedal connection open'
                self.connected = true

        pedal_remote.onmessage = (msg) ->
                e = JSON.parse msg.data
                console.log('event: ' + e)
                (self.buttons[ix].update(e[ix]) for ix in [0, 1, 2])
                self.timestamp = clock()
                handler(self)

        this  # freeze vs. connected mutability. hmm.

root = exports ? this
root.FootPad = FootPad
