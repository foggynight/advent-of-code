# 1.py - Robert Coffey - 2022-12-05

def group_lines(lines):
    groups = []
    group = []
    for line in lines:
        if line == "":
            groups.append(group)
            group = []
        else: group.append(line)
    groups.append(group)
    return groups

def sum_lines(group):
    acc = 0
    for l in group:
        acc += int(l)
    return acc

def sum_groups(groups):
    return [sum_lines(g) for g in groups]

def main():
    lines = [l.strip() for l in open("1.dat").readlines()]
    groups = group_lines(lines)
    sums = sum_groups(groups)
    print("Part 1:", max(sums))
    print(sorted(sums)[-3:])
    print("Part 2:", sum(sorted(sums)[-3:]))
