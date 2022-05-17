from cmath import sqrt
from dis import dis


def ans():
    n = int(input())
    qbString = input().split(" ")
    qbCoords = (int(qbString[0]), int(qbString[1]))

    mascotCoords = []
    for i in range(n):
        mascot = input().split(" ")
        mascotCoords.append((int(mascot[0]), int(mascot[1])))

    input() # uneccesary 0

    distances = {}
    for coord in mascotCoords:
        distance = (pow(coord[0] - qbCoords[0], 2) + pow(coord[1] - qbCoords[1], 2)) ** (1.0 / 2.0)
        rounded = round(distance)
        if(distances.get(rounded)):
            distances[rounded] += 1
        else:
            distances[rounded] = 1
    ropeLen = 0
    maxMascots = 0
    for len in distances:
        if distances[len] > maxMascots:
            maxMascots = distances[len]
            ropeLen = len
        elif distances[len] == maxMascots:
            ropeLen = min(ropeLen, len)

    print(ropeLen)


ans()