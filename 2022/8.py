# 8.py - Robert Coffey - 2022-12-08

def visible(grid, y, x):
    h, w = len(grid), len(grid[0])
    val = grid[y][x]
    def north():
        for wy in range(y):
            if grid[wy][x] >= val:
                return False
        return True
    def south():
        for wy in range(h-1, y, -1):
            if grid[wy][x] >= val:
                return False
        return True
    def west():
        for wx in range(x):
            if grid[y][wx] >= val:
                return False
        return True
    def east():
        for wx in range(w-1, x, -1):
            if grid[y][wx] >= val:
                return False
        return True
    return north() or south() or west() or east()

def view_score(grid, y, x):
    h, w = len(grid), len(grid[0])
    val = grid[y][x]
    def north():
        dist = 0
        for wy in range(y-1, -1, -1):
            dist += 1
            if grid[wy][x] >= val:
                break
        return dist
    def south():
        dist = 0
        for wy in range(y+1, h):
            dist += 1
            if grid[wy][x] >= val:
                break
        return dist
    def west():
        dist = 0
        for wx in range(x-1, -1, -1):
            dist += 1
            if grid[y][wx] >= val:
                break
        return dist
    def east():
        dist = 0
        for wx in range(x+1, w):
            dist += 1
            if grid[y][wx] >= val:
                break
        return dist
    return north() * south() * west() * east()

def main():
    grid = [list(l.rstrip()) for l in open("8.dat").readlines()]
    h, w = len(grid), len(grid[0])

    count = 0
    for y in range(h):
        for x in range(w):
            if visible(grid, y, x):
                count += 1

    print(count)

def main2():
    grid = [list(l.rstrip()) for l in open("8.dat").readlines()]
    h, w = len(grid), len(grid[0])

    scores = [[view_score(grid, y, x) for x in range(w)] for y in range(h)]
    max_score = max([max(row) for row in scores])

    print(max_score)
