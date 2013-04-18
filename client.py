import socket
import commands
import time

def hostname():
    return commands.getoutput("hostname").strip()

import random, sys

HOST, PORT = "localhost", 9999
if(len(sys.argv) > 1):
    HOST = sys.argv[1]
if(len(sys.argv) > 2):
    PORT = int(sys.argv[2])

while True:
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    try:
        # Connect to server and send data
        sock.connect((HOST, PORT))
        sock.sendall("GET %s\n" % hostname())
        # Receive data from the server and shut down
        received = sock.recv(1024)
        print(received)
        if(int(received.strip().split()[1]) == -1): break
    finally:
        sock.close()
    time.sleep(random.randint(1,4))
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    try:
        # Connect to server and send data
        sock.connect((HOST, PORT))
        sock.sendall("DONE %s\n" % hostname())
        # Receive data from the server and shut down
        received = sock.recv(1024)
        print(received)
    finally:
        sock.close()

