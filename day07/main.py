import itertools

with open('input') as f:
    lista = f.read().splitlines()

lista = [l.split(' ') for l in lista]
tiers = ['J', '2', '3', '4', '5', '6','7', '8', '9', 'T', 'Q','K', 'A']

def solve(tiers):
    result = []
    for i, l in enumerate(lista):
        value_order = [tiers.index(t) for t in l[0]]
        bet = int(l[1])

        hand = [list(g) for k, g in itertools.groupby(sorted(l[0]))]
        hand = sorted(hand, key = lambda x:(len(x)), reverse=True)

        for j in range(len(hand)):
            if 'J' in hand[j] and len(hand)!=1:
                to_add = len(hand.pop(j))
                hand = sorted(hand, key=lambda x:(len(x), tiers.index(x[0])), reverse=True)
                hand[0]+=[hand[0][0]]*to_add
                break

        if len(hand) == 1:
            tier=7
        elif len(hand)==2:
            if len(hand[0])==4:
                tier=6
            elif len(hand[0])==3:
                tier=5
                
        elif len(hand)==3:
                if len(hand[0])==3:
                    tier=4
                elif len(hand[0])==2 and len(hand[1])==2:
                    tier=3
                    
        elif len(hand)==4:
            if len(hand[0])==2:
                tier=2
                
        elif len(hand)==5:
                tier=1
        result.append((tier, value_order, bet))

    result=sorted(result, key=lambda x:(x[0], x[1]))
    win =0
    for i, s in enumerate(result):
        win+=(i+1)*s[2]
    return win

print(solve(tiers))
