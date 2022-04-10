from math import ceil


def ans():
    n = int(input())
    studentsInput = input().split(" ")
    students = []
    for i in range(n):
        students.append(int(studentsInput[i]))

    students.sort()
    occurences = {}
    for s in students:
        if occurences.get(s):
            occurences[s] += 1
        else:
            occurences[s] = 1

    maxOccurence = max(occurences, key = occurences.get)
    
    minSideOccurence = ceil(occurences[maxOccurence] / 2)
    halfSide = ceil(n/4)

    if halfSide < minSideOccurence:
        print("NO")
    else:
        print("YES")


ans()