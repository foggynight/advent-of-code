def find_common(s1, s2):
    for c in s1:
        if c in s2:
            return c
    return False

def find_badge(s1, s2, s3):
    for c in s1:
        if c in s2 and c in s3:
            return c
    return False

def get_priority(c):
    if c.islower(): return ord(c) - ord('a') + 1
    else:           return ord(c) - ord('A') + 27

def main():
    lines = [l.strip() for l in open("3.dat").readlines()]

    sacks = []
    for l in lines:
        end = len(l) // 2
        sacks.append((l[:end], l[end:]))

    acc = 0
    for sack in sacks:
        common = find_common(*sack)
        acc += get_priority(common)

    print(acc)

def main2():
    lines = [l.strip() for l in open("3.dat").readlines()]
    groups = [lines[i*3:i*3+3] for i in range(len(lines) // 3)]

    acc = 0
    for group in groups:
        badge = find_badge(*group)
        acc += get_priority(badge)

    print(acc)
