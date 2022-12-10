# 2.py - Robert Coffey - 2022-12-05

def score_round(opp, you):
    score = {"X": 1, "Y": 2, "Z": 3}[you]
    if opp == "A":
        if you == "X": score += 3
        elif you == "Y": score += 6
    if opp == "B":
        if you == "Y": score += 3
        elif you == "Z": score += 6
    if opp == "C":
        if you == "Z": score += 3
        elif you == "X": score += 6
    return score

def main():
    pairs = [l.split() for l in open("2.dat").readlines()]
    score = 0
    for pair in pairs:
        score += score_round(*pair)
    print(score)

plays = ["A", "B", "C"]

res_scores = {"X": 0, "Y": 3, "Z": 6}
play_scores = {"A": 1, "B": 2, "C": 3}

def score_round2(opp, res):
    if res == "X":   play = plays[(plays.index(opp) - 1) % 3]
    elif res == "Y": play = opp
    elif res == "Z": play = plays[(plays.index(opp) + 1) % 3]
    return res_scores[res] + play_scores[play]

def main2():
    pairs = [l.split() for l in open("2.dat").readlines()]
    score = 0
    for pair in pairs:
        score += score_round2(*pair)
    print(score)
