package horaceplot.plot;
import java.io.*;
import java.util.*;
import java.applet.*;

/**
 * Connection to the data server for plots.
 * 
 * @author Dave tiddeman (dave.tiddeman@metoffice.gov.uk)
 * @version 1
 */

public class plotconn extends horaceplot.horconn implements Runnable
{
private Thread CDthread;
private boolean b;
public byte mapstatus;
public short derindex,dercount;
public String flightnumber;
public final int nstatus=11;
public float[] status=new float[nstatus];
private int time;
public float[] maxs,mins;
public float max1,min1,max0,min0;
public Vector data=new Vector();
    int lasttime;
    public int delt=0;
    int[] parameters;
    int sleeptime;
    plotapp plotar=null;

	public plotconn(String hos, int por) throws IOException
	{
		super(hos, por);
	}
	public plotconn(Applet app) throws IOException
	{
		super(app);
	}

    public void addPoints(int n,float[] x,int stop){
        int np=x.length;
        if(np>0){
          dataset d=(dataset)data.elementAt(n);
          if(stop==-1)d.currentvalue=x[np-1];
          if(time>0){
            for(int i=0;i<np;i++){
              d.addPoint(x[i]);
            }
          }
        }
    }
    
    public void addPoints(int n,float[] x){
        dataset d=(dataset)data.elementAt(n);
        int np=x.length;
        for(int i=0;i<np;i++){
          d.addPoint(x[i]);
        }
    }
    public void addPara(int n,String name,String units){
        data.addElement(new dataset(n,name,units));
    }
    public int[] getparams(){
        int n=data.size();
        int[] p=new int[n];
        for(int i=0;i<n;i++){
          p[i]=((dataset)data.elementAt(i)).number;
        }
        return p;
    }
    public int getparam(int i){
        int p;
        p=((dataset)data.elementAt(i)).number;        
        return p;
    }
   public String event(String typ,String Name,String Comment){
     if(Name==null)Name="";
     if(Comment==null)Comment="";
     String ans=new String("");
     while(Name.length()<20)Name+=" ";
     while(Comment.length()<80)Comment+=" ";
     synchronized(this){
     try{
      writeString("EVENT"+typ.substring(0,1));
      writeString(Name.substring(0,20));
      writeString(Comment.substring(0,80));
      flush();
      ans=readString(25);
     }catch(IOException ioe){
       System.out.println(ioe);
     }
     }
     return ans;
   }
     


   /**
    * Read in data for some parameters
    * 
    * @param para Parameter numbers
    * @param tim Time for start
    * @param first First run through ?
    * @return Array of data values
    * @exception java.io.IOException Connection dropped
    */
   
   public float[][] getData(int[] para,int tim,int tim2,boolean first) throws IOException{
     float[][] ans;
     int np=para.length;
     if(first){
    maxs=new float[np];
    mins=new float[np];
     }
     synchronized(this){
        writeString("PARA");
        writeint(tim);
        writeint(tim2);
        writeint(np);
        System.out.print("PARA ");
        System.out.print(tim);
		System.out.print(" , ");
		System.out.print(tim2);
		System.out.print(" , ");
		System.out.println(np);
        for(int innn=0;innn<np;innn++){
			System.out.print(para[innn]);
			System.out.print(" , ");
        	writeint(para[innn]);}
        	System.out.println(" ) ");
        flush();   
          readStatus();
          time=reader.readInt();
          int nnx=reader.readInt();
		System.out.print("Time= ");
		System.out.print(time);
		System.out.print("nnx= ");
		System.out.println(nnx);
		System.out.print("np= ");
		System.out.println(np);
          ans=new float[np][nnx];
          for(int innn=0;innn<np;innn++){
          for(int inn=0;inn<nnx;inn++){
            float ax=reader.readFloat();
            ans[innn][inn]=ax;
            if((inn==0)&first){
                maxs[innn]=ax;
                mins[innn]=ax;
                if(innn==0){
                    max0=ax;
                    min0=ax;
                }
                if(innn==1){
                    max1=ax;
                    min1=ax;
                }
                if(innn>1){   
                  if(max1<ax)max1=ax;
                  if(min1>ax)min1=ax;
                }
            }else{
                if(time>-1){
                  if(maxs[innn]<ax)maxs[innn]=ax;
                  if(mins[innn]>ax)mins[innn]=ax;
                  if(innn==0){
                    if(max0<ax)max0=ax;
                    if(min0>ax)min0=ax;
                  }else{   
                    if(max1<ax)max1=ax;
                    if(min1>ax)min1=ax;
                  }
                }
            }
            
          }
          }
     }
        return ans;
    
   }

   /**
    * Read in *Status data
    * 
    * @exception java.io.IOException Connection dropped
    */
   
   public void readStatus() throws IOException{
       mapstatus=reader.readByte();
       derindex=reader.readShort();
       dercount=reader.readShort();
       for(int i=0;i<nstatus;i++){
         status[i]=reader.readFloat();
       }
       flightnumber=readString(4);
//       flightnumber=(new StringBuffer("B").append((int)status[11])).toString();
   }
    
   public void getStatus() throws IOException{
      synchronized(this){
       writeString("STAT");
       flush();
       readStatus();
      }
   }

   public void close(){
    synchronized(this){
       try{
       System.out.println("QUIT");
       writeString("QUIT");
       flush();
       }catch(IOException ioe){
        System.out.println(ioe);}
       super.close();
    }
   }
   public void stop(){
//     CDthread.stop();
     b=false;
//     try{Thread.sleep(sleeptime);}catch(Exception e){}
     CDthread=null;
     close();
   }
   public boolean start(plotapp plotapp,int start,int stop,int st){
    int[] params=getparams();
    int np=params.length;
    try{
        getStatus();
        if(start>0){
            start=derindex-(((int)status[0]-start));
            if(start<0)start=1;
            if(start>derindex)start=derindex;
        }
        if(stop>0){
            stop=derindex-(((int)status[0]-stop));
            if(stop<start)stop=start;
            if(stop>derindex)stop=derindex;
        }
        if(start==-2)start=1;
        if(params!=null){
            float[][] a=getData(params,start,stop,true);
            for(int i=0;i<np;i++){
              addPoints(i,a[i],stop);
            }
        }
         if(stop>0)time=-1;
         CDthread = new Thread(this);
         parameters=params;
         lasttime=time;
         sleeptime=st;
         plotar=plotapp;
         
         CDthread.start();
         return true;
    }catch(IOException ioe){
        System.out.println(ioe);
        return false;
    }
   }
    public void run(){
        int np=parameters.length;
        b=true;
        int count=0;
        while(b){
        try{Thread.sleep(sleeptime);
        try{
         if(parameters!=null){
          float[][] a=getData(parameters,lasttime,-1,false);
          delt=1*(time-lasttime); //the 1 is the size of the time steps, in secs
          lasttime=time;
          for(int i=0;i<np;i++){
            addPoints(i,a[i],-1);
          }
         }else{
            getStatus();
         }
		 if(plotar!=null)plotar.drawlatestData();
          
        }catch(IOException ioe){
        }
        }catch(InterruptedException ie){
            System.out.println("Horace Conn "+ie);
            b=false;
        }
        }
    }

}
