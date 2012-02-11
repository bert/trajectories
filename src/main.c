/*!
 * \file main.c
 *
 * \author Bert Timmerman <bert.timmerman@xs4all.nl>
 *
 * \brief Attempt to calculate some surfcasting related issues.
 *
 * \warning All units are SI: metres, kilograms and seconds.
 *
 * Problem Statement:\n
 * Given base \c b and height \c h, the length of a special segment on a
 * parabola can be computed as in the following formula:
 * \image html ../doc/p_length.jpg
 * \n
 * Following example Fortran code can be found at: \n\n
 * http://www.cs.mtu.edu/~shene/COURSES/cs201/NOTES/chap02/p-length.html
 * as long as it lives there.\n\n
 *
 * \code
 
! -----------------------------------------------------------
!   Calculate the length of a parabola given height and base.
! -----------------------------------------------------------

PROGRAM  ParabolaLength
IMPLICIT  NONE
 
REAL  :: Height, Base, Length
REAL  :: temp, t
 
WRITE(*,*)  'Height of a parabola : '
READ(*,*)   Height
 
WRITE(*,*)  'Base of a parabola   : '
READ(*,*)   Base
 
! ... temp and t are two temporary variables
 
t      = 2.0 * Height
temp   = SQRT(t**2 + Base**2)
Length = temp + Base**2/t*LOG((t + temp)/Base)
 
WRITE(*,*)
WRITE(*,*)  'Height = ', Height
WRITE(*,*)  'Base   = ', Base
WRITE(*,*)  'Length = ', Length
 
END PROGRAM  ParabolaLength
 
\endcode
 * 
 * Discussion of above Fortran code:\n
 * The values of base and height will be stored in REAL variables
 * \c Base and \c Height, respectively.\n
 * \c Length will be used to store the parabola segment length.\n
 * Since the content in the square root is used twice, it would be
 * more convenient to save the result in a variable.\n
 * This value will be stored in \c temp.\n
 * Since \c 2h also appears a few times, variable \c t is used to store
 * this value.\n
 * After reading in \c Height and \c Base, 2.0 * Height is computed and
 * stored in \c t with the first assignment.\n
 * Then, the second assignment computes the content in the square
 * root and stores the result into \c temp.\n
 * The third assignment compute the segment length and stores the
 * result into \c Length. \n
 * Note that intrinsic function \c LOG() is used.\n
 * The four WRITE statements display the input and the results. \n
 * \n
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *      
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *   
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <getopt.h>


#define VERSION "0.0.1"
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#define M180 (M_PI/180.0)
#define RAD_TO_DEG (180.0/M_PI)
#define TO_RADIANS(degrees) (M180 * (degrees))
#define GRAVITY 9.81
#define FALSE 0
#define TRUE 1


/* Me bad for having so much globals. */
double angle;
double velocity_0;
double velocity_0_y;
double velocity_0_x;
double time_of_flight;
double time_to_peak_height;
double range;
double peak_height;
double line_out;
double kinetic_energy_100g;
double kinetic_energy_125g;
double kinetic_energy_150g;
double kinetic_energy_175g;


/*!
 * \brief Print the usage message to stderr.
 *
 * \return \c EXIT_SUCCESS.
 */
int
print_usage ()
{
        fprintf (stderr, ("\ntrajectories usage and options:\n"));
        fprintf (stderr, ("\t --help \n"));
        fprintf (stderr, ("\t -? \n"));
        fprintf (stderr, ("\t -h        : print this help message and exit.\n\n"));
        fprintf (stderr, ("\t --verbose \n"));
        fprintf (stderr, ("\t -v        : log messages, be verbose.\n\n"));
        fprintf (stderr, ("\t --silent \n"));
        fprintf (stderr, ("\t --quiet \n"));
        fprintf (stderr, ("\t -q        : do not log messages, overrides --verbose.\n\n"));
        fprintf (stderr, ("\t --version \n"));
        fprintf (stderr, ("\t -V        : print the version information and exit.\n\n"));
        fprintf (stderr, ("\t --format <filename> \n"));
        fprintf (stderr, ("\t -f < filename>\n\n"));
        fprintf (stderr, ("\t --output <filename> \n"));
        fprintf (stderr, ("\t -o <filename>\n\n"));
        fprintf (stderr, ("\t --debug \n"));
        fprintf (stderr, ("\t -d        : turn on debugging output messages.\n\n"));
        return (EXIT_SUCCESS);
}


/*!
 * \brief Print the version to stderr.
 *
 * \return \c EXIT_SUCCESS.
 */
int
print_version ()
{
        fprintf (stderr, ("\ntrajectories version %s\n"), VERSION);
        fprintf (stderr, ("(C) 2012 by Bert Timmerman.\n"));
        fprintf (stderr, ("This free software is released under the GPL v2 license;\n"));
        fprintf (stderr, ("see the source for copying conditions.\n"));
        fprintf (stderr, ("There is NO warranty; not even for MERCHANTABILITY\n"));
        fprintf (stderr, ("or FITNESS FOR A PARTICULAR PURPOSE.\n\n"));
        return (EXIT_SUCCESS);
}


int
calculate ()
{
    double t;
    double temp;

    /* Do some arithmatic. */
    velocity_0_x = velocity_0 * cos (TO_RADIANS (angle));
    velocity_0_y = velocity_0 * sin (TO_RADIANS (angle));
    peak_height = (velocity_0_y * velocity_0_y) / (2 * GRAVITY);
    time_to_peak_height = velocity_0_y / GRAVITY;
    range = (2 * velocity_0_x * velocity_0_y) / GRAVITY;
    time_of_flight = (2 * velocity_0_y) / GRAVITY;
    t = 2 * peak_height;
    temp = sqrt ((t * t) + (range * range));
    line_out =  temp + ((range * range) / t) * log ((t + temp) / range);
    kinetic_energy_100g = 0.5 * 0.100 * velocity_0 * velocity_0;
    kinetic_energy_125g = 0.5 * 0.125 * velocity_0 * velocity_0;
    kinetic_energy_150g = 0.5 * 0.150 * velocity_0 * velocity_0;
    kinetic_energy_175g = 0.5 * 0.175 * velocity_0 * velocity_0;
    return (EXIT_SUCCESS);
}


int
print_report ()
{
    /* Now tell us what you found. */
    fprintf (stdout, "*** Calculate trajectories for surfcasting ***\n");
    fprintf (stdout, "\n");
    fprintf (stdout, "Input data.\n");
    fprintf (stdout, "Gravity :                         %f [m/s2]\n", GRAVITY);
    fprintf (stdout, "Starting velocity :               %f [m/s]\n", velocity_0);
    fprintf (stdout, "Angle :                           %f [degrees]\n", angle);
    fprintf (stdout, "\n");
    fprintf (stdout, "Output data.\n");
    fprintf (stdout, "Horizontal starting velocity :    %f [m/s]\n", velocity_0_x);
    fprintf (stdout, "Vertical starting velocity :      %f [m/s]\n", velocity_0_y);
    fprintf (stdout, "Peak height :                     %f [m]\n", peak_height);
    fprintf (stdout, "Time to peak height :             %f [s]\n", time_to_peak_height);
    fprintf (stdout, "Range :                           %f [m]\n", range);
    fprintf (stdout, "Total time of flight :            %f [s]\n", time_of_flight);
    fprintf (stdout, "Minimum required line on spool :  %f [m]\n", line_out);
    fprintf (stdout, "Minimum required kinetic energy\n");
    fprintf (stdout, "    for 100 gram :                %f [Joule]\n", kinetic_energy_100g);
    fprintf (stdout, "    for 125 gram :                %f [Joule]\n", kinetic_energy_125g);
    fprintf (stdout, "    for 150 gram :                %f [Joule]\n", kinetic_energy_150g);
    fprintf (stdout, "    for 175 gram :                %f [Joule]\n", kinetic_energy_175g);
    fprintf (stdout, "\n");
    return (EXIT_SUCCESS);
}


int
main (int argc, char *argv[])
{
    int debug;
    int silent;
    int verbose;
    /* Determine how we are called today */
    char *program_name = NULL;
    program_name = argv[0];
    static const struct option opts[] =
    {
        {"debug", no_argument, NULL, 'd'},
        {"help", no_argument, NULL, 'h'},
        {"version", no_argument, NULL, 'V'},
        {"verbose", no_argument, NULL, 'v'},
        {"quiet", no_argument, NULL, 'q'},
        {"silent", no_argument, NULL, 'q'},
        {"format", required_argument, NULL, 'f'},
        {"output", required_argument, NULL, 'o'},
        {0, 0, 0, 0}
    };
    int optc;
    while ((optc = getopt_long (argc, argv, "dhVvqqf:o:", opts, NULL)) != -1)
    {
        switch (optc)
        {
            case 'd':
                debug = TRUE;
                break;
            case 'h':
                print_usage ();
                exit (EXIT_SUCCESS);
            case 'V':
                print_version ();
                exit (EXIT_SUCCESS);
            case 'v':
                verbose = TRUE;
                break;
            case 'q':
                silent = TRUE;
                verbose = FALSE; /* Just to be sure. */
                break;
            case 'f':
                break;
            case 'o':
                break;
            case '?':
                print_usage ();
                exit (EXIT_FAILURE);
            default:
                fprintf (stderr, "unknown command line option encountered.\n");
                print_usage ();
                exit (EXIT_FAILURE);
        }
    }
    if (optind < argc)
    {
                print_usage ();
                exit (EXIT_FAILURE);
        }
    angle = 45.;
    velocity_0 = 50.;
    calculate ();
    print_report ();
    exit (EXIT_SUCCESS);
}


/* EOF */
