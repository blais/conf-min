#!/usr/bin/env python3
# Print a horizontal gradient to test truecolor support

for i in range(0, 256):
    r = i
    g = 255 - i
    b = (i // 2) + 128 if i < 256 else 255
    print(f"\033[48;2;{r};{g};{b}m ", end="")

print("\033[0m")  # reset colors

