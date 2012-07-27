
package horaceplot.plot;

import java.io.*;
import horaceplot.stringutl;
/**
 * Reads in data and writes to a file.
 * 
 * @author Dave tiddeman (dave.tiddeman@metoffice.gov.uk)
 * @version 1
 */

public class readdata
{
	// The following function is a placeholder for control initialization.
	// You should call this function from a constructor or initialization function.
	public void vcInit() {
		//{{INIT_CONTROLS
		//}}
	}
	
    public static int[] paras;
    public static int start,stop;
    public static int portno=1500;
    public static String hostname="192.168.101.71";
    public static plotconn HC;
    public static String fileout;
    private static DataOutputStream fwrite;
    private static PrintStream ps;
    
    public static void main(String[] args){
      if(args.length>=4){
       fileout=args[0];
       start=stringutl.secs(args[1]);
       stop=stringutl.secs(args[2]);
       int n=stringutl.countseps(args[3],",");
       paras=stringutl.stringsepi(args[3],",",n);
       if(args.length>=5)hostname=args[4];
       if(args.length>=6)portno=Integer.parseInt(args[5]);
       if(paras!=null){
        try{
          HC=new plotconn(hostname,portno);
          HC.getStatus();
         if(start>0)start=HC.derindex-(((int)HC.status[0]-start)/3);
          if(stop>0)stop=HC.derindex-(((int)HC.status[0]-stop)/3);
          if(start==-2)start=1;
		  System.out.print("Reading data from ");
		  System.out.println(hostname);
          float[][] a=HC.getData(paras,start,stop,true);
          HC.close();
          fwrite=null;
          ps=null;
          if(fileout.equals("system")){
          	ps=System.out;
//            fwrite=new DataOutputStream(System.out);
          }else{
	        File outfile=new File(fileout);
//	        fwrite=new DataOutputStream(new FileOutputStream(outfile));
	        ps=new PrintStream(new FileOutputStream(outfile));
	      }
	    
//	      fwrite.writeInt((int)a[0].length);
		  System.out.print("Writing data to ");
		  System.out.println(fileout);
          ps.print("Parameters ");
          ps.println(args[3]);
		  ps.print("Number of data points ");
          ps.println(a[0].length);

          int columnwidth=15;
    	  for(int i=0;i<a[0].length;i++){
    	    for(int i2=0;i2<paras.length;i2++){
    	    	StringBuffer s=new StringBuffer("");
    	    	s.append(a[i2][i]);
    	    	for(int ig=s.length();ig<columnwidth;ig++){
    	    		s.append(" ");
    	    	}
    	    	s.setLength(columnwidth);
    	        ps.print(s);
//    	        fwrite.writeFloat(a[i2][i]);
                if(i2<(paras.length-1))ps.print(",");
    	    }
    	    ps.println();
    	  }
//    	  fwrite.close();
    	  ps.close();
        }catch(IOException ioe){
          System.out.println(ioe);
        }
      }  
     }
     System.out.println("Finished");
    }
	//{{DECLARE_CONTROLS
	//}}
}
