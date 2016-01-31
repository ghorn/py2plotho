import json
import zmq
import time
import numpy as np

url = 'ipc:///tmp/py2plotho'

context = zmq.Context()
socket = context.socket(zmq.PUB)
socket.bind(url)
time.sleep(0.3)

count = 0
for k in range(10):
    otherMessage = {'otherValue':k*k, 'otherCount':count}
    socket.send_multipart(["topic1", json.dumps(otherMessage)])
    for j in range(10):
        msg = {'value':np.sin(count), 'count':count}
        socket.send_multipart(["topic0", json.dumps(msg)])
        count = count + 1
        time.sleep(0.1)
