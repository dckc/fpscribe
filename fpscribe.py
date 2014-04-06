from collections import namedtuple
import logging
import struct

log = logging.getLogger(__name__)


def main(argv, usb_hids):
    hid_dev = argv[1]
    fp = FootPedal(usb_hids.sub_rd(hid_dev))
    for e in fp.each_event():
        log.debug('event: %s', e)


class FootPedal(object):
    '''VEC USB Footpedal (USB device idVendor=05f3, idProduct=00ff)
    registers as hid device.

    ack: footpedal__ explains that each pedal action generates three
    groups of 8 characters.

    __ https://code.google.com/p/footpedal/
    '''

    pedal_qty = 3
    pedal_data_fmt = 'B3xB3x'
    pedal_data_size = struct.calcsize(pedal_data_fmt)
    event_size = pedal_data_size * pedal_qty

    def __init__(self, device):
        def each_event():
            stream = device.open_rd()
            while 1:
                data = stream.read(self.event_size)
                if not data:
                    break
                yield self.unpack(data)

        self.each_event = each_event

    @classmethod
    def unpack(cls, data):
        r'''Unpack footpedal event data.

        >>> chunks = [chr(ix) + '\x00\x00\x00\x01\x00\x00\x00'
        ...           for ix in range(1, 4)]
        >>> data = ''.join(chunks)
        >>> FootPedal.unpack(data)
        ... # doctest: +NORMALIZE_WHITESPACE
        [PedalEvent(ix=1, pressed=1),
         PedalEvent(ix=2, pressed=1),
         PedalEvent(ix=3, pressed=1)]
        '''
        pedals_data = [
            struct.unpack(cls.pedal_data_fmt,
                          data[ix:ix + cls.pedal_data_size])
            for ix in range(0, cls.event_size, cls.pedal_data_size)]
        return [PedalEvent(ix, status)
                for (ix, status) in pedals_data]


PedalEvent = namedtuple('PedalEvent', ['ix', 'pressed'])


# Read access rights.
Rd = namedtuple('Rd', ['path', 'sub_rd', 'open_rd'])


def hid_devs(openf,
             path='/dev/usb/hiddev'):
    def sub_rd(n):
        if not n.isdigit():
            raise IOError('not a device number: ' + n)
        return Rd(path + n, sub_rd=_cannot,
                  open_rd=lambda: openf(path + n))

    return Rd(path, sub_rd, open_rd=_cannot)


def _cannot(*args):
    raise IOError('not authorized: %s' % args)


def argv_rd(argv, rd):
    '''Attenuate a rd so only CLI args are allowed as sub paths.
    '''
    def sub_rd(n):
        if n not in argv:
            raise IOError('not CLI arg: ' + n)
        return rd.sub_rd(n)
    return Rd(rd.path, sub_rd, rd.open_rd)


if __name__ == '__main__':
    def _logging(level=logging.DEBUG):
        logging.basicConfig(level=level)

    def _with_caps():
        from __builtin__ import open as openf
        from sys import argv

        hid_rd = hid_devs(openf)
        main(argv, argv_rd(argv, hid_rd))

    _logging()
    _with_caps()
