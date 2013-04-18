import SocketServer
import Queue
import threading
import time

q = Queue.Queue()
lock = threading.Lock()

class MyTCPHandler(SocketServer.BaseRequestHandler):
    def handle(self):
        global q, lock
        # self.request is the TCP socket connected to the client
        self.data = self.request.recv(1024).strip()
        if(self.data.startswith("GET")):
            try:
                targetIP = q.get_nowait()
            except:
                targetIP = -1
            res = "%s %s (%d)\n" % (time.ctime(), self.data, targetIP)
            self.request.sendall("OK " + str(targetIP))
        else:
            res = "%s %s\n" % (time.ctime(), self.data)
            self.request.sendall("OK")

        with lock:
            with open("stat_server", "a+") as fd: 
                fd.write(res)
        # just send back the same data, but upper-cased

import sys

if __name__ == "__main__":
    HOST, PORT = "", 9999
    if(len(sys.argv) > 1):
        PORT = int(sys.argv[1])
    for i in range(256):
        q.put(i)

    # Create the server, binding to localhost on port 9999
    server = SocketServer.TCPServer((HOST, PORT), MyTCPHandler)

    # Activate the server; this will keep running until you
    # interrupt the program with Ctrl-C
    server.serve_forever()
