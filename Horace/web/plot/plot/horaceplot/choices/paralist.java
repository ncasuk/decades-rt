package horaceplot.choices;

import java.io.*;
import java.net.*;
import java.util.*;
import java.awt.*;

/**
 * A list of parameters
 * 
 * @author Dave Tiddeman
 * @version 1
 */

public class paralist
{

    /**
     * Vector of parameters
     * 
     * @see para
     */
    public Vector Paras;

	/**
	 * Creates a new list
	 * 
	 */
    public paralist(){
        Paras=new Vector();
    }
	/**
	 * Reads in a list of parameters from a file
	 * 
     * @param ind0 Column for start of number
     * @param ind1 Column for end of number
     * @param ind2 Column for start of short name
     * @param ind3 Column for end of short name
     * @param ind4 Column for start of full name
     * @param ind5 Column for end of full name / start of units
     * @param unit Default units if not in file
	 * @param textfile URL of file text file to read parameters from
     *
	 */ 
	     
    public paralist(URL textfile,int ind0,
    int ind1,int ind2,int ind3,int ind4,int ind5,String unit){
        Addparas(textfile,ind0,ind1,ind2,ind3,ind4,ind5,unit);
    }
 /**
  * Add a parameter to list
  * @param numb Parameter number
  * @param fnam Full name
  * @param sname Short name
  * @param unit Units
  */   
    public void Addpara(int numb,String fnam,String sname,String unit){                                
       para p=new para(numb,fnam,sname,unit);
       Paras.addElement(p);
    }
 
/**
 * Add parameters to list from file
     * @param ind0 Column for start of number
     * @param ind1 Column for end of number
     * @param ind2 Column for start of short name
     * @param ind3 Column for end of short name
     * @param ind4 Column for start of full name
     * @param ind5 Column for end of full name / start of units
     * @param unit Default units if not in file
	 * @param textfile URL of file text file to read parameters from
 */
    public void Addparas(URL textfile,int ind0,
    int ind1,int ind2,int ind3,int ind4,int ind5,String unit){
        if(Paras==null)Paras=new Vector();
            String Line;
            try{
                System.out.println(textfile);
                BufferedReader reader=new BufferedReader(new InputStreamReader(textfile.openStream()));
                boolean open=true;
                while(open){
                    try{
                        Line=reader.readLine();
                        if(Line!=null){
                      if(Line.length()>=(ind4+1)){
                        String num=Line.substring(ind0,ind1);
                            try{
                                int numb=Integer.parseInt(num.trim());
                                String sname=Line.substring(ind2,ind3);
                            String fnam;
                                if(Line.length()>=ind5){ 
                                  unit=Line.substring(ind5);
                                  fnam=Line.substring(ind4,ind5);
                                }else{
                                  fnam=Line.substring(ind4);
                                }
                                Addpara(numb,fnam,sname,unit);
                            }catch(NumberFormatException nfe){}
                      }
                        }else{
                            open=false;
                        }
                            }catch(IOException ioe){
                        open=false;
                    }
                }
                reader.close();
                    
            }catch(Exception e){
                System.out.println("failed");
            }       
        
    }
/**
 * Create a Choice
 * 
 * @param c Choice to create
 */

public void makechoice(Choice c){
     para f;
     for (Enumeration e = Paras.elements() ; e.hasMoreElements() ;) {
        f=(para)e.nextElement();
        c.add(f.toString());
     }
}
/**
 * Make a list
 * @param c List 
 */
public void makelist(java.awt.List c){
     para f;
     for (Enumeration e = Paras.elements() ; e.hasMoreElements() ;) {
        f=(para)e.nextElement();
        c.add(f.toString());
     }
}
/**
 * String representstion of one parameter
 * @param index
 * @return number.fullname(units)
 */
public String toString(int index){
    para p=getPara(index);
    return p.toString();
}
/**
 * Short string representation of one parameter
 * @param index
 * @return number.shortname
 */
public String shortString(int index){
    para p=getPara(index);
    return p.shortString();
}

/**
 * Get a parameter from the list
 * 
 * @param index Index in list
 * @return A parameter
 */
public para getPara(int index){
    para p=(para)Paras.elementAt(index);
    return p;
}

/**
 * Get a parameter name from the list
 * 
 * @param index Index in list
 * @return A parameter name
 */
public String getName(int index){
    para p=getPara(index);
    return p.fullname;
}

/**
 * Get a parameter number from the list
 * 
 * @param index Index in list
 * @return A parameter number
 */

public int getNumber(int index){
    para p=getPara(index);
    return p.number;
}

/**
 * Get a parameter's units from the list
 * 
 * @param index Index in list
 * @return A parameter units
 */
public String getUnits(int index){
    para p=getPara(index);
    return p.units;
}

/**
 * Get a parameter's shortname from the list
 * 
 * @param index Index in list
 * @return A parameter units
 */
public String getShortname(int index){
    para p=getPara(index);
    return p.shortname;
}

/**
 * Gets the index of a particular parameter number
 * 
 * @param Pnum Parameter number
 * @return Index of parameter number in list
 */

public int getIndex(int Pnum){
    int index=0;
    int i=0;
    para f;
     for (Enumeration e = Paras.elements() ; e.hasMoreElements() ;) {
        f=(para)e.nextElement();
        if(f.number==Pnum)index=i;
        i++;
     }
     return index;
}
    
/**
 * Holds info about one parameter
 * 
 * @author Dave Tiddeman
 * @version 1
 */
public class para
{
    public String fullname,shortname,units;
    public int number;

    /**
     * Creates a parameter
     * 
     * @param n Parameter number
     * @param fn Full name
     * @param sn Short name
     * @param un Units
     */
    
    public para(int n,String fn,String sn,String un){
      fullname=fn;
      shortname=sn;
      units=un;
      number=n;
    
                //{{INIT_CONTROLS
		//}}
        }

    /**
     * Converts to String
     * 
     * @return number.fullname(units)
     */
    public String toString(){
        String ans=number+"."+fullname+" ("+units+")";
        return ans;
    }
/**
 * Converts to a short string
 * @return number.shortname
 */    
    public String shortString(){
        String ans=number+"."+shortname;
        return ans;
    }
}
}
