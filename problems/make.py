import os
print(os.getcwd())
i = input("Project Euler number:")

program = f"""module euler{i}_mod
use euler_help
implicit none
contains


function euler{i} ()
	implicit none
	
	integer :: euler{i}

end function euler{i}

end module euler{i}_mod"""

with open(f"euler{i}.f95","w") as f:
    f.write(program)


