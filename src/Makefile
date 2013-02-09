CFLAGS = -Wall -g `pkg-config --cflags gtk+-2.0`
LDFLAGS = -lm `pkg-config --libs gtk+-2.0`

all: \
        trajectories

trajectories: main.c
	$(CC) -o trajectories main.c $(CFLAGS) $(LDFLAGS)
clean:
	rm -f *~
	rm -f *.o
	rm -f trajectories
