
package horaceplot.plot;

/**
 * A tick mark defines what writing goes with a tick at a 
 * particular value.
 * 
 * @author Dave tiddeman (dave.tiddeman@metoffice.gov.uk)
 * @version 1
 */

public class tickmark
{

    /**
     * Value of tick mark.
     */
    public float value;

    /**
     * Name of tick mark.
     */
    public String name;

    /**
     * Sets up a tick mark with any string as text.
     * 
     * @param val Value where the tick mark goes.
     * @param nam Text to go with tick mark.
     */
    
    public tickmark(float val,String nam){
        value=val;
        name=nam;
    }
    
    public tickmark(){
        value=0;
        name="";
    }

    /**
     * Sets up a tick mark for a time.
     * 
     * @param val Value where the tick mark goes.
     * @param time The time in seconds past midnight.
     */
    public tickmark(float val,boolean time){
        if(time){
           name=horaceplot.stringutl.gmt(val);
        }else{
           name=new StringBuffer("").append(val).toString();
        }
        value=val;
    }

  /**
   * Adds leading zeroes to a number.
   * 
   * @param a Number
   * @param n Number of digits in result.
   * @return String with leading zeroes added
   */
    


  /**
   * Tick mark as a string.
   * 
   * @return Name of tick mark.
   */
  public String toString(){
    return name;
  }
}
