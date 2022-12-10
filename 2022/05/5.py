# 5.py - Robert Coffey - 2022-12-05

def push(e, lst):
    return lst + [e]

def pop(lst):
    return lst[-1], lst[:-1]

def exec_step(step, stacks):
    [cnt, src, dst] = [step[0], step[1]-1, step[2]-1]
    for _ in range(cnt):
        e, s = pop(stacks[src])
        stacks[src] = s
        stacks[dst] = push(e, stacks[dst])

def main():
    lines = open("5.dat").readlines()

    stacks = [list(line.strip()) for line in lines[:9]]

    steps = []
    for line in lines[9:]:
        sp = line.split()
        steps.append([int(sp[1]), int(sp[3]), int(sp[5])])

    for step in steps:
        exec_step(step, stacks)

    print("".join([s[-1] for s in stacks]))

def move(cnt, src, dst):
    return src[:-cnt], dst + src[-cnt:]

def exec_step2(step, stacks):
    [cnt, src, dst] = [step[0], step[1]-1, step[2]-1]
    stacks[src], stacks[dst] = move(cnt, stacks[src], stacks[dst])

def main2():
    lines = open("5.dat").readlines()

    stacks = [list(line.strip()) for line in lines[:9]]

    steps = []
    for line in lines[9:]:
        sp = line.split()
        steps.append([int(sp[1]), int(sp[3]), int(sp[5])])

    for step in steps:
        exec_step2(step, stacks)

    print("".join([s[-1] for s in stacks]))
