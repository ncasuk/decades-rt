
package horaceplot.plot;
import java.util.*;
/**
 * Object for holding plotting data
 * @author Dave tiddeman (dave.tiddeman@metoffice.gov.uk)
 * @version 1
 */

public class dataset
{
/** Name of parameter */
    public String name;
	/** Name of units */
    public String units;
	/** Number of parameter */
    public int number;
	/** Colour of plot */
    public int colour;
	/** Symbol for plot */
    public int symbol;
	/** Data for plot */
   public Vector data;
   /** Current value */
    public float currentvalue;
	/** Number of data points */
    public int datalength;
    
    /** 
     * 
     * @param pn  Parameter number
     * @param n   Parameter name
     * @param u   Parameter units
     * @param c   Colour
     * @param s   Symbol
     */
    
    public dataset(int pn,String n, String u,int c,int s){
        data=new Vector();
        number=pn;
        units=u;
        name=n;
        datalength=0;
        colour=c;
        symbol=s;
    }
    
    /**
     * 
     * @param pn  Parameter number
     * @param n   Parameter name
     * @param u   Parameter units
     */
    public dataset(int pn,String n, String u){
        this(pn,n,u,0,-1);
    }
    /**
     * @param p  Data value to add to dataset
     */
    public void addPoint(float p){
        data.addElement(new Float(p));
        datalength++;
    }
    
    /**
     * 
     * @return  array of data
     */
    public float[] getdata(){
        int n=data.size();
        float[] a=new float[n];
        for(int i=0;i<n;i++){
            Float p=(Float)data.elementAt(i);
            a[i]=p.floatValue();
        }
        return a;
    }
    
    /**
     * 
     * @param pos  Position in dataset
     * @return     Extracted data
     */
    public float getdatapoint(int pos){
            Float p=(Float)data.elementAt(pos);
            return p.floatValue();
    }
    /**
     * 
     * @param start Data from point
     * @return array of data
     */
    public float[] getdata(int start){
        int n=data.size()-start;
        float[] a=new float[n];
        for(int i=0;i<n;i++){
            Float p=(Float)data.elementAt(i+start);
            a[i]=p.floatValue();
        }
        return a;
    }
}
