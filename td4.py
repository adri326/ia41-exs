#!/bin/env python

file_tree = {
    'rep1': {
        'rep3': {'auto': 0, 'moto': 0, 'velo': 0},
        'lolo': 0,
        'lulu': 0,
    },
    'rep2': {
        'rep4': {'fado': 0, 'java': 0, 'torot': 0},
        'chato': 0,
        'gato': 0,
        'rado': 0,
        'rato': 0,
    },
    'tata': 0,
    'toto': 0,
    'tutu': 0,
}

def file_dfs(tree):
    res = []
    for k, v in tree.items():
        if v == 0:
            if 'to' in k:
                res.append(k)
        else:
            res = res + file_dfs(v)
    return res

print(file_dfs(file_tree))

def file_bfs(tree):
    res = []
    open_list = [tree]
    while len(open_list) > 0:
        current = open_list.pop()
        if type(current) == str:
            if 'to' in current:
                res.append(current)
        else:
            for k, v in current.items():
                if v == 0:
                    open_list.insert(0, k)
                else:
                    open_list.insert(0, v)
    return res

print(file_bfs(file_tree))

# state = (missionaire_gauche, cannibales_gauche, radeau)

def missionaires_prod(state):
    m, c, r = state
    candidates = []
    if r == 0: # Left
        if m > 0 and c > 0:
            candidates.append((m-1, c-1, 1))
        if c > 0:
            candidates.append((m, c-1, 1))
        if c > 1:
            candidates.append((m, c-2, 1))
        if m > 0:
            candidates.append((m-1, c, 1))
        if m > 1:
            candidates.append((m-2, c, 1))
    else:
        if m < 3 and c < 3:
            candidates.append((m+1, c+1, 0))
        if c < 3:
            candidates.append((m, c+1, 0))
        if c < 2:
            candidates.append((m, c+2, 0))
        if m < 3:
            candidates.append((m+1, c, 0))
        if m < 2:
            candidates.append((m+2, c, 0))
    return filter(
        lambda s:
            (s[0] >= s[1] or s[0] == 0 or s[1] == 0)
            and ((3-s[0]) >= (3-s[1]) or s[0] == 3 or s[1] == 3),
        candidates)


closed = []
def missionaires(state, path):
    closed.append(state)
    if state[0] == 0 and state[1] == 0:
        return path
    else:
        best = None
        for child in missionaires_prod(state):
            if child in closed:
                continue
            x = missionaires(child, path + [child])
            if x != None:
                if best == None or len(x) < len(best):
                    best = x
        return best

print(missionaires((3, 3, 0), []))
