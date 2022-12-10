# 4.py - Robert Coffey - 2022-12-05

# Does p1 contain p2?
def contains(p1, p2):
    return p1[0] <= p2[0] and p1[1] >= p2[1]

def main():
    pairs = [l.strip().split(",") for l in open("4.dat").readlines()]
    range_pairs = [[[int(e) for e in r.split("-")] for r in p] for p in pairs]

    cnt = 0
    for rp in range_pairs:
        if contains(rp[0], rp[1]) or contains(rp[1], rp[0]):
            cnt += 1

    print(cnt)

# Does p1 contain any part of p2?
def contains2(p1, p2):
    return p1[0] <= p2[1] and p1[1] >= p2[0]

def main2():
    pairs = [l.strip().split(",") for l in open("4.dat").readlines()]
    range_pairs = [[[int(e) for e in r.split("-")] for r in p] for p in pairs]

    cnt = 0
    for rp in range_pairs:
        if contains2(rp[0], rp[1]):
            cnt += 1

    print(cnt)
