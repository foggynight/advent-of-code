# 7.py - Robert Coffey - 2022-12-07

class File:
    def __init__(self, name, size):
        self.name = name
        self._size = size

    def size(self):
        return self._size

class Dir:
    def __init__(self, name, parent, children):
        self.name = name
        self.parent = parent
        self.children = children
        self._size = None

    def size(self):
        if self._size is None:
            self._size = sum([c.size() for c in self.children])
        return self._size

# Construct a file tree from lines.
def eval_lines(lines):
    root = Dir("/", None, [])             # root directory

    cwd = root                            # current working directory
    ln = 1 if lines[0] == "$ cd /" else 0 # current line number

    def eval_cd(path):
        nonlocal cwd
        nonlocal ln
        ln += 1
        if path == "..":
            if cwd is root:
                exit("invalid cd: root has no parent")
            cwd = cwd.parent
        else:
            for c in cwd.children:
                if c.name == path:
                    cwd = c
                    return
            exit("invalid cd: could not find child: " + path)

    def eval_ls():
        nonlocal ln
        ln += 1
        while True:
            if ln >= len(lines): break
            expr = lines[ln].split()
            match expr:
                case ["$", *rest]:  break
                case ["dir", name]: child = Dir(name, cwd, [])
                case [size, name]:  child = File(name, int(size))
                case _: exit("invalid input: " + lines[ln])
            cwd.children.append(child)
            ln += 1

    while True:
        if ln >= len(lines): break
        expr = lines[ln].split()
        match expr:
            case ["$", "cd", path]: eval_cd(path)
            case ["$", "ls"]:       eval_ls()
            case _:                 exit("invalid command: " + lines[ln])

    return root

def print_tree(root, prefix = ""):
    if type(root) is File:
        print(f"{prefix}{root.size()} {root.name}")
    elif type(root) is Dir:
        print(f"{prefix}dir {root.name}")
        for child in root.children:
            print_tree(child, prefix + "    ")

def dir_info(root):
    info = []
    def _dir_info(tree):
        nonlocal info
        if type(tree) is Dir:
            dir_size = sum([_dir_info(c) for c in tree.children])
            info.append([tree.name, tree.size()])
    _dir_info(root)
    return info

def main(lines):
    root = eval_lines([l.rstrip() for l in lines])
    info = tree_info(root)

    less_100k = list(filter(lambda e: e[1] <= 100_000, info))
    less_100k_sum = sum([d[1] for d in less_100k])

    print_tree(root)
    print()
    print(less_100k_sum)

def main2(lines):
    root = eval_lines([l.rstrip() for l in lines])
    info = tree_info(root)

    info.sort(key=lambda e: e[1])
    print(info)

    disk_size = 70_000_000
    unused_size = disk_size - root.size()
    delta_size = 30_000_000 - unused_size

    small_dir = None
    for di in info:
        if di[1] >= delta_size:
            small_dir = di
            break

    print_tree(root)
    print("ROOT_SIZE:", root.size())
    print("DELTA_SIZE:", delta_size)
    print("SMALL_DIR:", *small_dir)

#main(open("7.dat").readlines())
main2(open("7.dat").readlines())
