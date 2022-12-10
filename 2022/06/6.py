def find_marker(data, cnt=4):
    for i in range(len(data)):
        sub = data[i:i+cnt]
        if len(set(sub)) == cnt:
            return i + cnt

def main():
    data = open("6.dat").read().rstrip()
    print(find_marker(data))

def main2():
    data = open("6.dat").read().rstrip()
    print(find_marker(data, 14))
