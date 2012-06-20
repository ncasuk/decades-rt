package horaceplot.drs;

import java.io.*;
import java.net.*;
import java.util.*;
import java.awt.*;

/**
 * A list of grouped parameters
 * 
 * @author Dave Tiddeman
 * @version 1
 */

public class grouparalist
{

    /**
     * Vector of groups
     * @see group
     */
    public Vector Groups;
    public Enumeration pointer;

    /**
     * Reads in a list of parameters from a file drsgroups.txt
     * 
     */
	public grouparalist(){
		Addparas("drsgroups.txt");
	}
	/**
	 * Reads in a list of parameters from a file
	 * 
	 * @param textfile URL of file text file to read parameters from
	 */
	public grouparalist(String textfile){
		Addparas(textfile);
	}
     
    public grouparalist(URL textfile){    
      Addparas(textfile);
    }
 
   public void Addparas(String textfile){
   	try{
   		Addparas(new URL(textfile));
   	}catch(Exception ioe){
   		System.out.println(ioe);
   	}
   }

    public void Addparas(URL textfile){
        Groups=new Vector();
            String Line,Line2;
            try{
                System.out.println(textfile);
                BufferedReader reader=new BufferedReader(new InputStreamReader(textfile.openStream()));
                boolean open=true;
                while(open){
                    try{
                        Line=reader.readLine();
                        if(Line!=null){
                          Line2=reader.readLine();
                          if(Line2!=null){
                 System.out.println(Line+" "+Line2);
                          	Groups.addElement(new group(Line,Line2));
                        }else{
                            open=false;
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
        
                //{{INIT_CONTROLS
		//$$ para1.move(0,0);
		//}}
        }

/**
 * Create a Choice from a list of parameters
 * 
 * @param c Choice to create
 */

/**
 * Create a Choice from a list of parameters
 * 
 * @param c Choice to create
 */

public void makechoice(Choice c){
     group f;
     for (Enumeration e = Groups.elements() ; e.hasMoreElements() ;) {
        f=(group)e.nextElement();
        c.add(f.toString());
     }
}

public void setpointer(int x){
	group f;
	int i;
	i=0;
	pointer=Groups.elements();
	while(i<x){
		f=(group)pointer.nextElement();
		i++;
	}
}

public int[] getparas(){
	group f;
	f=(group)pointer;
	return f.Paras;
}

public int[] getparas(int x){
	group f;
	int i;
	i=0;
	Enumeration e;
	e=Groups.elements();
	f=(group)e.nextElement();
	while(i<x){
		f=(group)e.nextElement();		
		i++;
		System.out.println(i);
		System.out.println(f);
	}
	return f.Paras;
}

   
public class group{
    	

     public String name;
     public int[] Paras;
     
     public group(String nme, int[] ps){
     	name=nme;
     	Paras=ps;
     }
     
     public group(String nme, String ps){
     	name=nme;
     	Paras=horaceplot.stringutl.stringsepi(ps,",",0); 
     }
     
     public String toString(){
       return name;
     }
    
        }

}
