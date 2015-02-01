import pickle
import ipaddress

with open("akamai.pcl", "rb") as f:
    lst = pickle.load(f)

lst.sort()

for i in lst[:1000]:
    addr = str(ipaddress.IPv4Address(i))
    print(addr) 
