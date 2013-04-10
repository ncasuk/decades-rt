
package horaceplot.plot;

import java.awt.*;
import java.applet.*;
import java.io.*;
import horaceplot.stringutl;
import java.util.Properties;

/**
 * The applet that produces the plots.
 * 
 * @author Dave tiddeman (dave.tiddeman@metoffice.gov.uk)
 * @version 1
 */

public class plotapp extends Applet implements java.awt.event.MouseListener
{
 String plottype,sstart,sstop;
 int[] paras,cols,syms;
 Dimension d;
 boolean initing=true;
 public int top=110;
 String[] pnames,punits;
 int start,stop;
 int count1=0;
 public int fact=14,oldfact=14;
 int norm=14;
 boolean sze=false;
 int portno=1500;
 String hostname="192.168.101.71";
 String mapdata="http://192.168.101.71/plot/mapdata.dat";
// java.awt.Color backg=new Color((int)12,(int)33,(int)116);
// java.awt.Color foreg=new Color((int)228,(int)242,(int)59);
 java.awt.Color backg=Color.white;
 java.awt.Color foreg=new Color((int)12,(int)33,(int)116);
// Image logoImage,logoImage2;
 Image offScreenBuffer;
 public plotconn HC;
 public Checkbox checkscroll;
 
       /* (non-Javadoc)
	 * @see java.applet.Applet#init()
	 */
	public void init()
        {
 // logoImage = getImage(getCodeBase(),"logo.jpg");
 //  logoImage2 =logoImage.getScaledInstance(1000/fact,1000/fact,Image.SCALE_SMOOTH);
               plottype=getParameter("plottype");
                String sparas=getParameter("parameters");
                String snames=getParameter("names");
                String sunits=getParameter("units");
                String sfact=getParameter("printfactor");
                // add hostname parameter
                String shostname=getParameter("hostname");
                if(shostname!=null) this.hostname=shostname;
                if(sfact!=null)fact=Integer.parseInt(sfact);
                oldfact=0;
                String snorm=getParameter("normalfactor");
                if(snorm!=null)norm=Integer.parseInt(snorm);
                String map=getParameter("mapdata");
//                map="http://condor/newhor/plot/map_data.dat";
                if(map!=null){
                    mapdata=map;
                }else{
                   try{
                     String s=this.getCodeBase().toString();
                     mapdata=s.substring
                       (0,s.lastIndexOf("/"))+"/map_data.dat";
                   }catch(Exception e){}
                }
                System.out.println(mapdata);
                sstart=getParameter("start");
                sstop=getParameter("stop");
                String plotsym=getParameter("symbols");
                String plotcol=getParameter("colours");
//                if(sparas==null){
//                    sparas="606,605";
//                    snames="a,b,c";
//                    sunits="a,b,c";
//                    sstart="start";
//                    sstop="now";
//                    plotsym="-1";
//                    plotcol="1,2";
//                    sze=true;
//                }
                start=stringutl.secs(sstart);
                stop=stringutl.secs(sstop);
                int n=stringutl.countseps(sparas,",");
                if(n>0){
                  paras=stringutl.stringsepi(sparas,",",n);
                  pnames=stringutl.stringsep(snames,",",n);
                  punits=stringutl.stringsep(sunits,",",n);
                  syms=stringutl.stringsepi(plotsym,",",n-1);
                  cols=stringutl.stringsepi(plotcol,",",n-1);
                }
               
                if(plottype==null){plottype="map";}
                if(plottype.equals("hodograph")){plot1=new hodoplot();}
                if(plottype.equals("tephigram")){plot1=new tphiplot();}
                if(plottype.equals("mxplot")){plot1=new xyplot1(true);}        
                if(plottype.equals("myplot")){plot1=new xyplot1(false);}        
                if(plottype.equals("map")){plot1=new mapplot(mapdata);}       
                if(plottype.equals("table")){
        try{
        	System.out.println("TABLE");
        HC=new plotconn(this);
        }catch(IOException ioe){}
		System.out.println("Paras=");
		System.out.println(paras);
        if(paras!=null){
			System.out.println(paras.length);
          for(int i=0;i<paras.length;i++){
			System.out.println(paras[i]);
            HC.addPara(paras[i],pnames[i],punits[i]);
          }
        }
         HC.start(this,-1,-1,300);
        
                }            
                String zooms=getParameter("zooms");
                if(zooms==null){
		try{
		netscape.javascript.JSObject win=netscape.javascript.JSObject.getWindow(this); 
		netscape.javascript.JSObject doc = (netscape.javascript.JSObject) win.getMember("document");
		netscape.javascript.JSObject fom = (netscape.javascript.JSObject) doc.getMember("form1");
		netscape.javascript.JSObject zm = (netscape.javascript.JSObject) fom.getMember("zm");
		zooms = (String)zm.getMember("value");
		}catch(Exception e){}
		        }

         
                // This code is automatically generated by Visual Cafe when you add
                // components to the visual environment. It instantiates and initializes
                // the components. To modify the code, only use code syntax that matches
                // what Visual Cafe can generate, or Visual Cafe may be unable to back
                // parse your Java file into its visual environment.
                //{{INIT_CONTROLS
                setLayout(null);
//                setSize(600,800);
                //}}
               setBackground(backg);
               System.out.println("Diagnose d");
               System.out.println(d);
               d=getSize();
			   System.out.println(d);
               top=110;
                if(plot1!=null){
                    top=d.height-200;
                    add(plot1);
                    int checkevery=1000;
//                    if(plottype.equals("map")){checkevery=5000;
//                    }
                    plot1.initialize(this,start,stop,checkevery,paras,pnames,punits,cols,syms);
                    HC=plot1.q;
//                   plot1.setBounds(100/norm,1100/factor,
//                (d.width*10-200)/factor,(d.height*10-3000)/factor);
                  if(zooms!=null){                    
                    String[] s=stringutl.stringsep(zooms,",",0);
                    if(s.length==4){
                    plot1.xmax=Float.valueOf(s[0]).floatValue();    
                    plot1.xmin=Float.valueOf(s[1]).floatValue();    
                    plot1.ymax=Float.valueOf(s[2]).floatValue();    
                    plot1.ymin=Float.valueOf(s[3]).floatValue();    
                    }
                  }
                }
                    
        this.addMouseListener(this);
		//{{REGISTER_LISTENERS
		//}}
		initing=false;
		repaint();
	}	

        
 	public float getxmax(){return (float)plot1.xmax; }
	public float getxmin(){return (float)plot1.xmin; }
	public float getymax(){return (float)plot1.ymax; }
	public float getymin(){return (float)plot1.ymin; }

    public String getzooms(){
        StringBuffer sb=new StringBuffer();
        sb.append((float)plot1.xmax).append(',');
        sb.append((float)plot1.xmin).append(',');
        sb.append((float)plot1.ymax).append(',');
        sb.append((float)plot1.ymin);
        return sb.toString();
    }
    
    public String gettimes(){
        StringBuffer sb=new StringBuffer();
        if(plottype.equals("mxplot")&&paras[0]==515){
          sb.append(stringutl.gmt(plot1.xmin)).append('-');       
          sb.append(stringutl.gmt(plot1.xmax));   
        }
        return sb.toString();
    }
    
    public void changezooms(){
    		try{
		netscape.javascript.JSObject win=netscape.javascript.JSObject.getWindow(this); 
		netscape.javascript.JSObject doc = (netscape.javascript.JSObject) win.getMember("document");
		netscape.javascript.JSObject fom = (netscape.javascript.JSObject) doc.getMember("form1");
		netscape.javascript.JSObject zm = (netscape.javascript.JSObject) fom.getMember("zm");
        String zom=getzooms();
		zm.setMember("value",zom);
		}catch(Exception e){}
	}

     
    public void draw(Graphics g, int factor){
      if(!initing){
        if(plot1!=null){
            if(oldfact!=factor){
                oldfact=factor;
                System.out.println("Diagnostics");
                System.out.println(factor);
                System.out.println(d);
                plot1.setBounds(100/factor,1100/factor,
                  (d.width*10-200)/factor,(d.height*10-3000)/factor);
                plot1.repaint();
            }
        }
        drawback(g,factor);
        writenumbers(g,factor);
        drawHead(g,factor);
      }
    }
        
        
    public void print(Graphics g){
        System.out.println("Printing");
        synchronized(this){
                draw(g,fact);
         }
 
    }
    public void paint(Graphics g){
        synchronized(this){
          draw(g,norm);
        }
    }

public void update(Graphics g)

    {
    synchronized(this){
    Graphics gr; 
    // Will hold the graphics context from the offScreenBuffer.
    // We need to make sure we keep our offscreen buffer the same size
    // as the graphics context we're working with.
 if (offScreenBuffer==null ||
                (! (offScreenBuffer.getWidth(this) == this.getSize().width
                && offScreenBuffer.getHeight(this) == this.getSize().height)))
        {
        offScreenBuffer = this.createImage(getSize().width, getSize().height);
        }
    // We need to use our buffer Image as a Graphics object:
    gr = offScreenBuffer.getGraphics();
    gr.setClip(0,0,getSize().width,getSize().height);
    paint(gr); // Passes our off-screen buffer to our paint method, which,
               // unsuspecting, paints on it just as it would on the Graphics
               // passed by the browser or applet viewer.
    g.drawImage(offScreenBuffer, 0, 0, this);
               // And now we transfer the info in the buffer onto the
               // graphics context we got from the browser in one smooth motion.
    }
    }
    
    public void drawHead(Graphics g,int factor){
        if(HC!=null){
        Graphics gr;
        Image bufferim=null;
        if(factor!=norm){
          gr=g;
        }else{
          bufferim=this.createImage(5000/factor,850/factor);
          gr=bufferim.getGraphics();
        }
        gr.setColor(backg);
        gr.setPaintMode();
        gr.fillRect(0,0,5000/factor,850/factor);
        gr.setColor(foreg);
        StringBuffer hdr=new StringBuffer();
//        hdr.append("Flight B").append((int)HC.status[11]);
        hdr.append("Flight ").append(HC.flightnumber);
        hdr.append("   "+stringutl.gmt(HC.status[0]));
        gr.setFont(getfon(factor,Font.BOLD));
        gr.drawString(hdr.toString(),200/factor,200/factor);
        hdr=new StringBuffer().append("Heading ");
        hdr.append((int)HC.status[1]).append(" deg  Speed ");
        hdr.append((int)HC.status[4]).append(" knots  Height ");
        hdr.append(HC.status[3]).append("kft   Press ");
        hdr.append((int)HC.status[2]).append("mb");
        gr.setFont(getfon(factor,Font.PLAIN));
        gr.drawString(hdr.toString(),200/factor,400/factor);
        Properties properties = System.getProperties();
        hdr=new StringBuffer();
//        hdr.append("Lat ");
//        StringBuffer l=new StringBuffer();
//        if(HC.status[9]>=0){
//            l=l.append(HC.status[9]).append("N");
//        }else{
//            l=l.append(-HC.status[9]).append("S");
//        }
//        hdr.append(l+"  Long ");
//        l=new StringBuffer();
//        if(HC.status[10]>=0){
//            l=l.append(HC.status[10]).append("E");
//        }else{
//            l=l.append(-HC.status[10]).append("W");
//        }
		hdr.append("Lat ").append(LatLong(HC.status[9],"NS"));
		hdr.append("Long ").append(LatLong(HC.status[10],"EW"));
		hdr.append("  Wind ");
        hdr.append((int)HC.status[7]).append(" ms-1/ ");
        hdr.append((int)HC.status[8]).append(" deg");
        gr.drawString(hdr.toString(),200/factor,600/factor);
        hdr=new StringBuffer().append("Temp ");
        hdr.append(HC.status[5]).append("C  Dewpoint ");
        hdr.append(HC.status[6]).append("C");
        hdr.append(properties.getProperty("file.encoding") + " fe ");
        hdr.append(properties.getProperty("file.encoding.pkg") + " fep ");
        hdr.append(properties.getProperty("sun.io.unicode.encoding") + " siue ");
        hdr.append(properties.getProperty("sun.jnu.encoding") + " sje ");
        gr.drawString(hdr.toString(),200/factor,800/factor);
        if(bufferim!=null)g.drawImage(bufferim,0,0,this);
        }
    }
    
    public void drawback(Graphics g,int factor){
       if(plot1!=null) g.drawRect(100/factor-1,1100/factor-1,
                  (d.width*10-200)/factor+2,(d.height*10-3000)/factor+2);

        int[] x1={250/factor,300/factor,350/factor,400/factor,450/factor};
        int[] y1=new int[5];
        g.setPaintMode();
//        if(factor==norm){
//          g.drawImage(logoImage,((d.width*10-1000)/factor),0,this);
//        }else{
//          g.drawImage(logoImage2,((d.width*10-1000)/factor),0,this);
//        }
        g.setColor(foreg);
        g.setFont(getfon(factor,Font.PLAIN));
        if(plot1!=null)g.drawString("From "+sstart+" to "+sstop,200/factor,1000/factor);
        if(paras!=null){
        g.drawString("Current values",700/factor,(top*10+250)/factor);
        for(int f=0;f<paras.length;f++){
            g.setColor(foreg);
            g.drawString(pnames[f],600/factor,(top*10+400+150*f)/factor);
            g.drawString(punits[f],4100/factor,(top*10+400+150*f)/factor);
          if((f>0)&&(plot1!=null)){
            g.setColor(java.awt.Color.white);
            g.fillRect(200/factor,(top*10+300+150*f)/factor,300/factor,150/factor);
            for(int i=0;i<y1.length;i++)y1[i]=(top*10+370+150*f)/factor;
            g.setColor(plot1.colors[cols[f-1]]);
            plot1.drawSome(g,x1,y1,syms[f-1]);
          }
        }
        }
    }
    
    public void stop(){
        HC.stop();
//        if(plot1!=null)plot1.stop();
        super.stop();
    }
        
        
    public String event(String typ,String Nam,String Comm){
        return HC.event(typ,Nam,Comm);
    }
    
    public String getTime(int secs){
        String ans="";
        if(HC!=null){
            ans=stringutl.gmt(HC.status[0]-secs);
        }
        return ans;
    }
    
    
        
    public void drawlatestData(){
        Graphics g=getGraphics();
        if(g!=null){
        count1++;
        if(count1>15){
            repaint();
//            if(plot1!=null)plot1.repaint();
            count1=0;
        }else{
        if(plot1!=null)plot1.drawlatestData();
       synchronized(this){
          drawHead(g,norm);
          writenumbers(g,norm);
          }
        }
        }
    }

    public Font getfon(int factor,int style){
       Font fon=new Font("Gill Sans MT",style,150/factor);
       return fon;
    }
    
    public void writenumbers(Graphics g,int factor){
        if(HC!=null){
//        StringBuffer t;
        Graphics gr;
        int dx=0;
        int dy=0;
        Image bufferim=null;
        if(paras!=null){
        if(factor!=norm){
          gr=g;
          dx=3400/factor;
          dy=(top*10+300)/factor;
        }else{
          bufferim=this.createImage(700/factor,(150*paras.length)/factor);
          gr=bufferim.getGraphics();
        }
        gr.setPaintMode();
        gr.setColor(backg);
        gr.fillRect(dx,dy,700/factor,(150*paras.length)/factor);
        gr.setColor(foreg);
        gr.setFont(getfon(factor,Font.PLAIN));
        for(int f=0;f<paras.length;f++){
//            t=new StringBuffer().append(((dataset)HC.data.elementAt(f)).currentvalue);
            gr.drawString(getnumber(f),dx,dy+(100+150*f)/factor);
        }
        if(bufferim!=null){
          g.drawImage(bufferim,
           3400/factor,(top*10+300)/factor,
           this);
        }
        }
        }
    }  
    public String LatLong(float x,String type ){
		StringBuffer l=new StringBuffer();
		double xx=(double)x;
		int ix=0;
		if(xx<0){
			xx=-xx;
			ix=1;
		}
		l.append((int)xx);
		l.append("°");
		xx=Math.IEEEremainder(xx,1.0);
		if(xx<0){xx+=1.0;}
		xx*=60.0;
		java.text.DecimalFormat DF=new java.text.DecimalFormat("#0.0");
		try {
			 l.append(DF.format(xx));
		   } catch (Exception pe) { }
    	l.append("'");
		l=l.append(type.substring(ix,ix+1));
		return l.toString();    	
    }
    public String getnumber(int f){
      java.text.DecimalFormat DF=new java.text.DecimalFormat("0.##");
      float ff=((dataset)HC.data.elementAt(f)).currentvalue;
      String t="";
      try {
           t=(DF.format(ff));
         } catch (Exception pe) { }
//      StringBuffer t=new StringBuffer().append(((dataset)HC.data.elementAt(f)).currentvalue);
//      int n=t.length()-3;
//      System.out.println("tlength1="+n);
//       if(n>50){for(int i=0;i<t.length();i++){
//        if(t.charAt(i)==('.'))n=i;
//       }
//      }
//      System.out.println("tlength2="+n);
//      t.setLength(n+3);
//      return t.toString();
        return t;
    }
    
	//{{DECLARE_CONTROLS
	zoomplot plot1;
	//}}


	public void mouseClicked(java.awt.event.MouseEvent event)
	{
			Object object = event.getSource();
		    repaint();
	}
	public void mousePressed(java.awt.event.MouseEvent event)
	{
	}
	public void mouseReleased(java.awt.event.MouseEvent event)
	{
	}
	public void mouseEntered(java.awt.event.MouseEvent event)
	{
	}
	public void mouseExited(java.awt.event.MouseEvent event)
	{
	}


}
