import numpy as np

def vp2dp(vp, p, temp):
    """Water vapour to dew point conversion using the formula from the 
    Buck CR2 hygrometer manual p19f. Returns the dew point in K.
    
    :param vp: vapour pressure in mb
    :param p: p: air pressure in mbar
    :param temp: air temperature in Kelvin

    :result: dew point temperature in K
    
    """
    a, b, c, d = (6.1121, 18.678, 257.14, 234.5)
    ef=1.0+10**-4*(2.2+p/10.*(0.0383+6.4*10**-5*(temp-273.15)*2))
    s=np.log(vp/ef)-np.log(a)
    result=d/2.0 * (b-s-((b-s)**2-4*c*s/d)**0.5)
    return result+273.15


def vp2fp(vp, p, temp):
    """Water vapour to frost point conversion using the formula from the 
    Buck CR2 hygrometer manual p19f. Returns the dew point in K.
    
    :param vp: vapour pressure in mb
    :param p: air pressure in mbar
    :param temp: air temperature in Kelvin
    : return: frost point temperature in K
    
    """
    a, b, c, d=(6.1115, 23.036, 279.82, 333.7)
    ef=1.0+10**-4*(2.2+p/10.*(0.0383+6.4*10**-5*(temp-273.15)*2))
    s=np.log(vp/ef)-np.log(a)
    result=d/2.0 * (b-s-((b-s)**2-4*c*s/d)**0.5)
    return result+273.15
