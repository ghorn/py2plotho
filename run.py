import json
import zmq
import time
import numpy as np

url = 'ipc:///tmp/py2plotho'

context = zmq.Context()
socket = context.socket(zmq.PUB)
socket.bind(url)
time.sleep(0.3)

for k in range(100):
    msg = {'value':np.sin(k), 'count':k}
    # print "sending",k
    socket.send(json.dumps(msg))
    time.sleep(0.1)
