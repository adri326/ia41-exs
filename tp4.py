def next_states(s):
    m, c, r = s
    candidates = []
    if r: # Right
        if m < 3 and c < 3:
            candidates.append((m+1, c+1, False))
        if c < 3:
            candidates.append((m, c+1, False))
        if c < 2:
            candidates.append((m, c+2, False))
        if m < 3:
            candidates.append((m+1, c, False))
        if m < 2:
            candidates.append((m+2, c, False))
    else: # Left
        if m > 0 and c > 0:
            candidates.append((m-1, c-1, True))
        if c > 0:
            candidates.append((m, c-1, True))
        if c > 1:
            candidates.append((m, c-2, True))
        if m > 0:
            candidates.append((m-1, c, True))
        if m > 1:
            candidates.append((m-2, c, True))

    # Filter the candidates to only retain those that are valid (ie. with no missionary eaten)
    return filter(
        lambda s:
            (s[0] >= s[1] or s[0] == 0 or s[1] == 0)
            and ((3-s[0]) >= (3-s[1]) or s[0] == 3 or s[1] == 3),
        candidates)

bfs_nodes = 0
def bfs(state, target = (0, 0, True), use_closed = True):
    closed = []
    open_list = [(state, [])]
    global bfs_nodes
    bfs_nodes = 0
    while len(open_list) > 0:
        bfs_nodes += 1
        current, path = open_list.pop()
        for child in next_states(current):
            if use_closed and child in closed:
                continue # Child node already encountered, ignoring it
            if child == target:
                return path + [child]
            closed.append(child)
            open_list.insert(0, (child, path + [child]))
    return None # If no solution was found :(

dfs_nodes = 0
def dfs(state, target = (0, 0, True), use_closed = True, max_len = 14):
    global dfs_nodes
    dfs_nodes = 0
    if use_closed:
        closed = []
    else:
        closed = None
    return dfs_recurse(state, [], target, closed, max_len)

def dfs_recurse(state, path, target, closed, max_len):
    global dfs_nodes
    dfs_nodes += 1
    if len(path) > max_len: # Arbitrary max depth :)
        return None
    if closed != None:
        closed.append(state)
    if state == target:
        return path
    else:
        best = None
        for child in next_states(state):
            if closed != None and child in closed:
                continue # Child node already encountered, ignoring it
            x = dfs_recurse(child, path + [child], target, closed, max_len)
            if x != None:
                if best == None or len(x) < len(best):
                    best = x
        return best

def iddfs(state, target = (0, 0, True), use_closed = True):
    global dfs_nodes
    dfs_nodes = 0
    for depth in range(1, 128):
        if use_closed:
            closed = []
        else:
            closed = None
        res = dfs_recurse(state, [], target, closed, depth)
        if res != None:
            return res
    return None

# Test BFS
assert bfs((3, 3, False)) != None # There should be a solution
assert bfs((3, 3, False)).pop() == (0, 0, True) # The last position should be the final state
assert len(bfs((0, 2, False))) == 1 # (0, 2, False) -> (0, 0, 1)
assert bfs((0, 1, True)) == [(0, 2, False), (0, 0, True)]

# Test DFS
assert dfs((3, 3, False)) != None # There should be a solution
assert dfs((3, 3, False)).pop() == (0, 0, True) # The last position should be the final state
assert len(dfs((0, 2, False), (0, 0, True), False)) == 1 # (0, 2, False) -> (0, 0, 1)
assert dfs((0, 1, True)) == [(0, 2, False), (0, 0, True)]

# Test IDDFS
assert iddfs((3, 3, False)) != None # There should be a solution
assert iddfs((3, 3, False)).pop() == (0, 0, True) # The last position should be the final state
assert len(iddfs((0, 2, False), (0, 0, True), False)) == 1 # (0, 2, False) -> (0, 0, 1)
assert iddfs((0, 1, True)) == [(0, 2, False), (0, 0, True)]

# Print stats for the different algorithms

bfs((3, 3, False))
print("BFS with occurence list: ", bfs_nodes)
bfs((3, 3, False), (0, 0, True), False)
print("BFS without occurence list: ", bfs_nodes)

dfs((3, 3, False))
print("DFS with occurence list: ", dfs_nodes)
dfs((3, 3, False), (0, 0, True), False, 14)
print("DFS without occurence list (d=14): ", dfs_nodes)

iddfs((3, 3, False))
print("IDDFS with occurence list: ", dfs_nodes)
iddfs((3, 3, False), (0, 0, True), False)
print("IDDFS without occurence list: ", dfs_nodes)
