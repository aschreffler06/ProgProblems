def ans():
    line1 = input().split(" ")
    numCubes = int(line1[0])
    radius = float(line1[1])
    volSphere = (4.0/3.0) * 3.141592653589793 * (radius * radius * radius)
    volCubes = []

    for i in range(numCubes):
        cubeDimensions = input().split(" ")
        l = float(cubeDimensions[0])
        w = float(cubeDimensions[1])
        h = float(cubeDimensions[2])
        volCubes.append(((l * w * h), h))

    volCubes.sort(key = lambda c:c[1])

    for cube in volCubes:
        if radius > (cube[1] / 2):
            volSphere += cube[0]
            radius = (((3/4) * volSphere) / 3.141592653589793) ** (1.0/3.0)
        else:
            print("We need to rebuild this!")
            return

    print("It's going to be a good set!")

ans()
