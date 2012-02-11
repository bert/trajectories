CFLAGS = -Wall
LDFLAGS = -lm

all: \
        trajectories

trajectories: main.c
	$(CC) -o trajectories main.c $(CFLAGS) $(LDFLAGS)
clean:
	rm -f *~
	rm -f *.o
	rm -f trajectories
