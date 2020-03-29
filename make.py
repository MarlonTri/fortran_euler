import os
import sys

eulers = [e[:-4] for e in os.listdir("../problems") if "f95" in e]
eulers = sorted(eulers,key=lambda x: int(x[5:]))
includes = "\n".join([f"use {e}_mod" for e in eulers])
prints = "\n".join([f'\tprint*,"{e}: ", '+(f'{e}()' if (e[5:] not in sys.argv) else '"SKIPPED"')for e in eulers])

program = f"""program euler_main

{includes}
use euler_help

implicit none     

   {prints}
   
end program euler_main"""

with open("euler_main.f95","w") as f:
    f.write(program)

cmd = ("g95 -o a.out ../euler_help.f95 "+" ".join([f"../problems/{e}.f95" for e in eulers]) +
       " euler_main.f95 && a.out")

with open("run.bat","w") as f:
    f.write(cmd)
