import fileinput
from typing import Tuple, List, NamedTuple
from enum import Enum
from collections import Counter


def readinputfile(inputfile: str) -> Tuple[str, ...]:
    """ Read all lines from inputfile and store them in a tuple of strings, with removed newlines. """
    lines = []
    for line in fileinput.input(inputfile):
        lines.append(line.rstrip())
    return tuple(lines)


def printresultline(day, result):
    print(f'Day {day:>3}: {result}')


class HandType(Enum):
    FIVE = 7
    FOUR = 6
    FULL_HOUSE = 5
    THREE = 4
    TWO_PAIR = 3
    ONE_PAIR = 2
    HIGH_CARD = 1


class Hand(NamedTuple):

    cardsstr: str
    cards: List[str]
    bid: int
    type: HandType
    sortKey: str

    @classmethod
    def calcType(cls, cards: List[str]) -> HandType:
        c = Counter(cards)
        values = list(c.values())
        values = sorted(values)
        # print(c)
        # print(values)
        if 5 in values:
            return HandType.FIVE
        elif 4 in values:
            return HandType.FOUR
        elif 3 in values:
            if 2 in values:
                return HandType.FULL_HOUSE
            else:
                return HandType.THREE
        elif 2 in values:
            if values[1] == 2:
                return HandType.TWO_PAIR
            else:
                return HandType.ONE_PAIR
        else:
            return HandType.HIGH_CARD

    @classmethod
    def createSortKey(cls, cardsstr: str, handType: HandType) -> str:
        cardsstr = cardsstr.replace('T', 'V').replace('J', 'W').replace('Q', 'X').replace('K', 'Y').replace('A', 'Z')
        return str(handType.value) + cardsstr


def main():
    # filename = 'inputfiles/day07_example.txt'
    filename = 'inputfiles/day07_input.txt'

    hands: List[Hand] = []
    for line in readinputfile(filename):
        # print(line)
        cardsstr, bid = line.split()
        cards = list(cardsstr)
        handType = Hand.calcType(cards)
        sortKey = Hand.createSortKey(cardsstr, handType)
        hand = Hand(cardsstr, cards, int(bid), handType, sortKey)
        hands.append(hand)
        # print(hand)

    # print()
    # for hand in hands:
    #     print(hand)
    # print()
    hands = sorted(hands, key=lambda hand: hand.sortKey)
    total = 0
    rank = 0
    for hand in hands:
        rank = rank + 1
        # print(f'{rank:4d} {hand.cardsstr} {hand.bid:4d} {hand.type.value}')
        total = total + (rank * hand.bid)

    printresultline('07a', total)

if __name__ == '__main__':
    main()
