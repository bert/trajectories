/*!
 * \file main.c
 * \author Bert Timmerman <bert.timmerman@xs4all.nl>
 * \brief Attempt to calculate some surfcasting related issues.
 * 
 * Problem Statement:\n
 * Given base \c b and height \c h, the length of a special segment on a
 * parabola can be computed as in the following formula:
 * \image html ../doc/p_length.jpg
 * \n
 * Following example Fortran code can be found at: \n\n
 * http://www.cs.mtu.edu/~shene/COURSES/cs201/NOTES/chap02/p-length.html
 * as long as it lives there.\n\n
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

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <config.h>


#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#define M180 (M_PI/180.0)
#define RAD_TO_DEG (180.0/M_PI)
#define TO_RADIANS(degrees) (M180 * (degrees))
#define GRAVITY 9.81


int
main (int argc, char *argv[])
{
    double angle;
    double velocity_0;
    double casting_weight;
    double velocity_0_y;
    double velocity_0_x;
    double time_of_flight;
    double time_to_peak_height;
    double range;
    double peak_height;
    double line_out;
    double t;
    double temp;
    double kinetic_energy;

    velocity_0_x = velocity_0 * cos (TO_RADIANS (angle));
    velocity_0_y = velocity_0 * sin (TO_RADIANS (angle));
    peak_height = (velocity_0_y * velocity_0_y) / (2 * GRAVITY);
    time_to_peak_height = velocity_0_y / GRAVITY;
    range = (2 * velocity_0_x * velocity_0_y) / GRAVITY;
    time_of_flight = (2 * velocity_0_y) / GRAVITY;
    t = 2 * peak_height;
    temp = sqrt ((t * t) + (range * range));
    line_out =  temp + ((range * range) / t) * log ((t + temp) / range);
    kinetic_energy = 0.5 * casting_weight * velocity_0 * velocity_0;

    fprint (stdout, "*** Calculate trajectories for surfcasting ***\n");
    fprint (stdout, "\n");
    fprint (stdout, "Input data.\n");
    fprint (stdout, "Gravity :                         %f [m/s2]\n", GRAVITY);
    fprint (stdout, "Casting weight :                  %f [grams]\n", casting_weight);
    fprint (stdout, "Starting velocity :               %f [m/s]\n", velocity_0);
    fprint (stdout, "Angle :                           %f [degrees]\n", angle);
    fprint (stdout, "\n");
    fprint (stdout, "Output data.\n");
    fprint (stdout, "Horizontal starting velocity :    %f [m/s]\n", velocity_0_x);
    fprint (stdout, "Vertical starting velocity :      %f [m/s]\n", velocity_0_y);
    fprint (stdout, "Peak height :                     %f [m]\n", peak_height);
    fprint (stdout, "Time to peak height :             %f [s]\n", time_to_peak_height);
    fprint (stdout, "Range :                           %f [m]\n", range);
    fprint (stdout, "Total time of flight :            %f [s]\n", time_of_flight);
    fprint (stdout, "Minimum required line on spool :  %f [m]\n", line_out);
    fprint (stdout, "Minimum required kinetic energy : %f [Joule]\n", kinetic_energy);
    fprint (stdout, "\n");
}


/* EOF */
