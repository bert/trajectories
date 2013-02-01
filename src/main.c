/*!
 * \file main.c
 *
 * \author Bert Timmerman <bert.timmerman@xs4all.nl>
 *
 * \brief Attempt to calculate some surfcasting related issues.
 *
 * \warning All units are SI: metres, kilograms and seconds.
 *
 * It is assumed that the sinker follows a trajectory similar to a parabola.\n
 * Given the base (\c range ) and the height (\c peak_height ), the length
 * (\c line_out ) of a special segment on a  parabola can be computed as in the
 * following formula:\n
 * \image html ../doc/p-length.jpg
 * \n
 * <hr>
 * <h1>Copyright Notice</h1>
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n
 * See the GNU General Public License for more details.\n
 * \n
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.\n
 * <hr>
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
#define GRAVITY 9.81 /* [m/s2] */
#define FPM 0.3048 /* [ft/m] */
#define FALSE 0
#define TRUE 1


/* Me bad for having so much globals. */
float angle;
        /*!< inclination with the horizontal plane.*/
float velocity_0;
        /*!< velocity at start of the trajectory (at this moment all the
         * stored energy in the rod is released).*/
double velocity_0_x;
        /*!< horizontal component of start velocity.*/
double velocity_0_y;
        /*!< vertical component of start velocity.*/
double time_of_flight;
        /*!< time of flight.*/
double time_to_peak_height;
        /*!< time to reach the highest elevation.*/
double range;
        /*!< range.*/
double peak_height;
        /*!< highest elevation reached.*/
double line_out;
        /*!< the length of the trajectory.*/
double kinetic_energy_100g;
        /*!< minimum required kinetic energy at start for a 100 grams
         * weight.*/
double kinetic_energy_125g;
        /*!< minimum required kinetic energy at start for a 123 grams
         * weight.*/
double kinetic_energy_150g;
        /*!< minimum required kinetic energy at start for a 150 grams
         * weight.*/
double kinetic_energy_175g;
        /*!< minimum required kinetic energy at start for a 175 grams
         * weight.*/
double kinetic_energy_200g;
        /*!< minimum required kinetic energy at start for a 200 grams
         * weight.*/
double angular_acceleration;
        /*!< angular acceleration needed to reach starting velocity.*/
char *casting_style;
        /*!< everybody has a favourite one.*/
double wind_velocity_x;
        /*!< horizontal component of wind velocity.*/
double wind_velocity_y;
        /*!< vertical component of wind velocity.*/
int wind_direction;
        /*!< wind direction divided in 30 degrees segments (like clock hours).*/

#define STYLE_OVERHEAD_THUMP 0
#define STYLE_BRIGHTON 1
#define STYLE_BACK_CAST 2
#define STYLE_OFF_THE_GROUND 3
#define STYLE_SOUTH_AFRICAN 4
#define STYLE_LOW_PENDULUM 5
#define STYLE_HIGH_PENDULUM 6

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
        fprintf (stderr, ("\t -h             : print this help message and exit.\n\n"));
        fprintf (stderr, ("\t --verbose \n"));
        fprintf (stderr, ("\t -v             : log messages, be verbose.\n\n"));
        fprintf (stderr, ("\t --silent \n"));
        fprintf (stderr, ("\t --quiet \n"));
        fprintf (stderr, ("\t -q             : do not log messages, overrides --verbose.\n\n"));
        fprintf (stderr, ("\t --version \n"));
        fprintf (stderr, ("\t -V             : print the version information and exit.\n\n"));
        fprintf (stderr, ("\t --format <filetype> \n"));
        fprintf (stderr, ("\t -f < filetype> : output format, default is ASCII.\n\n"));
        fprintf (stderr, ("\t --input <filename> \n"));
        fprintf (stderr, ("\t -i <filename>\n\n"));
        fprintf (stderr, ("\t --output <filename> \n"));
        fprintf (stderr, ("\t -o <filename>  : default is STDOUT.\n\n"));
        fprintf (stderr, ("\t --debug \n"));
        fprintf (stderr, ("\t -d        : turn on debugging output messages.\n\n"));
        fprintf (stderr, ("\t --imperial \n"));
        fprintf (stderr, ("\t -z        : turn on imperial output.\n\n"));
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
    velocity_0_x = (double) velocity_0 * cos (TO_RADIANS ((double) angle));
    velocity_0_y = (double) velocity_0 * sin (TO_RADIANS ((double) angle));
    peak_height = (velocity_0_y * velocity_0_y) / (2 * GRAVITY);
    time_to_peak_height = velocity_0_y / GRAVITY;
    range = (2 * velocity_0_x * velocity_0_y) / GRAVITY;
    time_of_flight = (2 * velocity_0_y) / GRAVITY;
    t = 2 * peak_height;
    temp = sqrt ((4 * (peak_height * peak_height)) + (range * range));
    line_out =  temp + (((range * range) / (2 * peak_height)) * log ((peak_height + temp) / range));
    kinetic_energy_100g = 0.5 * 0.100 * velocity_0 * velocity_0;
    kinetic_energy_125g = 0.5 * 0.125 * velocity_0 * velocity_0;
    kinetic_energy_150g = 0.5 * 0.150 * velocity_0 * velocity_0;
    kinetic_energy_175g = 0.5 * 0.175 * velocity_0 * velocity_0;
    kinetic_energy_200g = 0.5 * 0.200 * velocity_0 * velocity_0;
    return (EXIT_SUCCESS);
}


int
print_report ()
{
    /* Now tell us what you found. */
    fprintf (stdout, "*** Calculate trajectories for surfcasting ***\n");
    fprintf (stdout, "\n");
    fprintf (stdout, "** Input data. **\n");
    fprintf (stdout, "\n");
    fprintf (stdout, "Gravity :                         %f [m/s2]\n",
        GRAVITY);
    fprintf (stdout, "Starting velocity :               %f [m/s]\n",
        velocity_0);
    fprintf (stdout, "Angle :                           %f [degrees]\n",
        angle);
    fprintf (stdout, "\n");
    fprintf (stdout, "\n");
    fprintf (stdout, "** Output data. **\n");
    fprintf (stdout, "\n");
    fprintf (stdout, "WARNING: wind, friction and drag effects are excluded.\n");
    fprintf (stdout, "\n");
    fprintf (stdout, "Horizontal starting velocity :    %f [m/s]\n",
        velocity_0_x);
    fprintf (stdout, "Vertical starting velocity :      %f [m/s]\n",
        velocity_0_y);
    fprintf (stdout, "Peak height :                     %f [m]\n",
        peak_height);
    fprintf (stdout, "Time to peak height :             %f [s]\n",
        time_to_peak_height);
    fprintf (stdout, "Range :                           %f [m]\n",
        range);
    fprintf (stdout, "Total time of flight :            %f [s]\n",
        time_of_flight);
    fprintf (stdout, "\n");
    fprintf (stdout, "Minimum required line on spool :  %f [m]\n",
        line_out);
    fprintf (stdout, "Minimum required kinetic energy\n");
    fprintf (stdout, "    for 100 gram :                %f [Joule]\n",
        kinetic_energy_100g);
    fprintf (stdout, "    for 125 gram :                %f [Joule]\n",
        kinetic_energy_125g);
    fprintf (stdout, "    for 150 gram :                %f [Joule]\n",
        kinetic_energy_150g);
    fprintf (stdout, "    for 175 gram :                %f [Joule]\n",
        kinetic_energy_175g);
    fprintf (stdout, "    for 200 gram :                %f [Joule]\n",
        kinetic_energy_200g);
    fprintf (stdout, "\n");
    return (EXIT_SUCCESS);
}


int
print_imperial_report ()
{
    /* Now tell the imperial users what you found. */
    fprintf (stdout, "*** Calculate trajectories for surfcasting ***\n");
    fprintf (stdout, "\n");
    fprintf (stdout, "** Input data. **\n");
    fprintf (stdout, "\n");
    fprintf (stdout, "Gravity :                         %f [ft/s2]\n",
        GRAVITY / FPM / FPM);
    fprintf (stdout, "Starting velocity :               %f [ft/s]\n",
        velocity_0 / FPM);
    fprintf (stdout, "Angle :                           %f [degrees]\n",
        angle);
    fprintf (stdout, "\n");
    fprintf (stdout, "\n");
    fprintf (stdout, "** Output data. **\n");
    fprintf (stdout, "\n");
    fprintf (stdout, "WARNING: wind, friction and drag effects are excluded.\n");
    fprintf (stdout, "\n");
    fprintf (stdout, "Horizontal starting velocity :    %f [ft/s]\n",
        velocity_0_x / FPM);
    fprintf (stdout, "Vertical starting velocity :      %f [ft/s]\n",
        velocity_0_y / FPM);
    fprintf (stdout, "Peak height :                     %f [ft]\n",
        peak_height / FPM);
    fprintf (stdout, "Time to peak height :             %f [s]\n",
        time_to_peak_height);
    fprintf (stdout, "Range :                           %f [ft]\n",
        range / FPM);
    fprintf (stdout, "Total time of flight :            %f [s]\n",
        time_of_flight);
    fprintf (stdout, "\n");
    fprintf (stdout, "Minimum required line on spool :  %f [ft]\n",
        line_out / FPM);
    fprintf (stdout, "\n");
    return (EXIT_SUCCESS);
}


int
main (int argc, char *argv[])
{
    int debug;
    int silent;
    int verbose;
    int imperial;
    char *format_filetype;
    char *input_filename;
    char *output_filename;
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
        {"imperial", no_argument, NULL, 'z'},
        {"format", required_argument, NULL, 'f'},
        {"input", required_argument, NULL, 'i'},
        {"output", required_argument, NULL, 'o'},
        {0, 0, 0, 0}
    };
    int optc;
    imperial = FALSE;
    while ((optc = getopt_long (argc, argv, "dhVvqqzf:i:o:", opts, NULL)) != -1)
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
            case 'z':
                imperial = TRUE;
                break;
            case 'f':
                format_filetype = strdup (optarg);
                if (debug)
                {
                    fprintf (stderr, "format filetype = %s\n",
                        format_filetype);
                }
                break;
            case 'i':
                input_filename = strdup (optarg);
                if (debug)
                {
                    fprintf (stderr, "input filename = %s\n",
                        input_filename);
                }
                break;
            case 'o':
                output_filename = strdup (optarg);
                if (debug)
                {
                    fprintf (stderr, "output filename = %s\n",
                        output_filename);
                }
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
    fprintf (stdout, "Give an angle in degrees : ");
    fscanf (stdin, "%f", &angle);
    fprintf (stdout, "\n");
    if (angle == 0)
    {
        angle = 45.0;
    }
    if (imperial)
    {
        fprintf (stdout, "Give a velocity in ft/s : ");
        fscanf (stdin, "%f", &velocity_0);
        velocity_0 = velocity_0 * FPM;
        fprintf (stdout, "\n");
    }
    else
    {
        fprintf (stdout, "Give a velocity in m/s : ");
        fscanf (stdin, "%f", &velocity_0);
        fprintf (stdout, "\n");
    }
    if (velocity_0 == 0.0)
    {
        velocity_0 = 50.0;
    }

/*! \todo Implement Casting style. */

/*
    fprintf (stdout, "Choice of casting styles : \n");
    fprintf (stdout, "  0 = Overhead thump\n");
    fprintf (stdout, "  1 = Brighton\n");
    fprintf (stdout, "  2 = Back cast\n");
    fprintf (stdout, "  3 = Off The Ground\n");
    fprintf (stdout, "  4 = South African\n");
    fprintf (stdout, "  5 = Low pendulum\n");
    fprintf (stdout, "  6 = High pendulum\n");
    fprintf (stdout, "Give a casting style : ");
    fscanf (stdin, "%s", casting_style);
    fprintf (stdout, "\n");
*/
    if (velocity_0 == 0.0)
    {
        velocity_0 = 50.0;
    }
    calculate ();
    if (imperial)
    {
        print_imperial_report ();
    }
    else
    {
        print_report ();
    }
    exit (EXIT_SUCCESS);
}


/* EOF */
