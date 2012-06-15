

package horaceplot.plot;

import java.awt.*;
import java.awt.Graphics;
import java.util.*;


/**
 * Plot X Y graph
 * 
 * @author Dave tiddeman (dave.tiddeman@metoffice.gov.uk)
 * @version 1
 */

public class xyplot1 extends zoomplot implements java.awt.event.ItemListener
{    
   private int scalewidth=50;
   private int plotx,ploty;
   public boolean scrollingtime=false;
   public boolean independentx=true;
   public float[][] mhistory=new float[2][maxzooms+1];
   public float[] mhist=new float[4];
   int mhistpos;
   Checkbox checkscroll;
   Checkbox[] whichone;
   CheckboxGroup which;
   int whichline=0;
   float[] ym,yc;
   plotapp pq;
   boolean movewhichone=false;

    /**
     * Create an X,Y plot with particular maximums and minimums
     * 
     * @param xmx Maximum X
     * @param xmn Minimum X
     * @param ymx Maximum Y
     * @param ymn Minimum Y
     */
   
    public xyplot1(float xmx,float xmn,float ymx,float ymn){
        super(xmx,xmn,ymx,ymn);
    
		//{{INIT_CONTROLS
		setSize(0,0);
		//}}
	}

    /**
     * Create an X,Y plot
     * xyplot1(36000,0,100,0)
     */
    public xyplot1(){
        super(36000,0,100,0);
    }

   /**
    * Create an X,Y plot
    * 
    * @param indepx Boolean independent x or y (true=x)
    */
   
   public xyplot1(boolean indepx){
        this();
        independentx=indepx;
   }

   /**
    * Start plot
    */
   
   public void init(){
    Dimension d=getSize();
    plotx=d.width-scalewidth;
    ploty=d.height-scalewidth;
    setBackground(Color.white);
    setPlotBounds(scalewidth,0,plotx,ploty);
    float t1=1000;
          pq=(plotapp)getParent();
          int dx=5000/pq.norm;
          int dy=(pq.top*10+300)/pq.norm;
        if(independentx){
          setmaxmin(q.max0,q.min0,q.max1,q.min1);
          int addon=0;
          if((int)((dataset)(q.data.elementAt(0))).number==515){
//            pq.addscrollcheck();
            checkscroll=new Checkbox("scroll time",true);
            scrollingtime=true;
            alwaysredraw=true;
            pq.add(checkscroll);
            checkscroll.addItemListener(this);
            checkscroll.setBounds(dx+600/pq.norm,dy,1200/pq.norm,150/pq.norm);
            addon=1300/pq.norm;
          }
        }else{
          setmaxmin(q.max1,q.min1,q.max0,q.min0);
        }
          int n=q.maxs.length;
          whichone=new Checkbox[n];
          ym=new float[n-1];
          yc=new float[n-1];
          which=new CheckboxGroup();
          for(int i=0;i<n;i++){
            if(i==0){
              whichone[i]=new Checkbox("All",true,which);
            }else{
              ym[i-1]=1;
              yc[i-1]=0;
              whichone[i]=new Checkbox("",false,which);                
            }
            pq.add(whichone[i]);
            whichone[i].setBounds(dx,dy,600/pq.norm,150/pq.norm);
            whichone[i].addItemListener(this);
            dy+=150/pq.norm;
          }
    
   }

   /**
    * Draw the background
    * 
    * @param g Graphics context
    */
   
   public void drawBackground(Graphics g){
    g.setFont(new Font("SansSerif",Font.BOLD,fontSize));
       int i1=1;
        int i2=0;
        float d0=q.max0-q.min0;
        float d1=q.max1-q.min1;
        float max0=q.max0+d0/10;
        float max1=q.max1+d1/10;
        float min0=q.min0-d0/10;
        float min1=q.min1-d1/10;
        if(independentx){
            i1=0;
            i2=1;
            i2=(whichline==0)?1:whichline;
    setDatamaxmin(max0,min0,max1,min1);
        }else{
    setDatamaxmin(max1,min1,max0,min0);
            i1=(whichline==0)?1:whichline;
        }

//    setDatamaxmin(q.maxs[i1],q.mins[i1],q.maxs[i2],q.mins[i2]);
    Dimension d=getSize();
    plotx=d.width-scalewidth;
    ploty=d.height-scalewidth;
    setPlotBounds(scalewidth,0,plotx,ploty);
    g.setPaintMode();
    g.setColor(Color.black);
    StringBuffer sb;
    g.setClip(0,0,scalewidth,ploty+10);
    Vector v;
	int wl=(whichline>0)?(whichline-1):0;
    if(!independentx){
        v=getTickmarks(ymin,ymax);
    }else{
        g.setColor(colors[colours[wl]]);
        v=getTickmarks((ymin-yc[wl])/ym[wl],
                       (ymax-yc[wl])/ym[wl]);
    }
    int iy=v.size()/2;
    for(int i=0;i<v.size();i++){
        tickmark t=(tickmark)v.elementAt(i);
    if(!independentx){
        d=convert(0,t.value);
     }else{
        d=convert(0,t.value*ym[wl]+yc[wl]);
     }
    g.drawString(t.name,5,d.height);
        if((i==iy)){g.drawString(getunits(i2),2,d.height+15);}
    }
   
   g.setClip(scalewidth,0,plotx,ploty);
   g.setColor(Color.lightGray);
   for(int i=0;i<v.size();i++){
        tickmark t=(tickmark)v.elementAt(i);
    if(!independentx){
        d=convert(0,t.value);
     }else{
        d=convert(0,t.value*ym[wl]+yc[wl]);
     }
        g.drawLine(scalewidth,d.height,plotx+scalewidth,d.height);
    }
    double dd=(double)((ymax-ymin)/10);
    double qr=choosetick(dd,10,4,40,2,2);
    double m1=qr*(Math.floor(ymin/qr));
    g.setColor(Color.black);
    if((independentx))g.setColor(colors[colours[wl]]);
    while((float)m1<ymax){
        d=convert(0,(float)m1);
        g.drawLine(scalewidth,d.height,scalewidth+5,d.height);        
        m1+=qr;
    }
    g.setColor(Color.black);
    g.setClip(scalewidth-10,ploty,10+plotx,scalewidth);
    if((independentx)&(q.getparam(0)==515)){
        v=getTimemarks(xmin,xmax);
    }else{
      if(independentx){
        v=getTickmarks(xmin,xmax);
      }else{
        g.setColor(colors[colours[wl]]);
        v=getTickmarks((xmin-yc[wl])/ym[wl],
                       (xmax-yc[wl])/ym[wl]);
      }
    }
    for(int i=0;i<v.size();i++){
        tickmark t=(tickmark)v.elementAt(i);
        if(independentx){
          d=convert(t.value,0);
        }else{
          d=convert(t.value*ym[wl]+yc[wl],0);
        }
        g.drawString(t.name,d.width,ploty+15);
    }
   g.drawString(getunits(i1),plotx/2,ploty+30);
   g.setClip(scalewidth,0,plotx,ploty);
   g.setColor(Color.lightGray);
   for(int i=0;i<v.size();i++){
        tickmark t=(tickmark)v.elementAt(i);
        if(independentx){
          d=convert(t.value,0);
        }else{
          d=convert(t.value*ym[wl]+yc[wl],0);
        }
        g.drawLine(d.width,0,d.width,ploty);
    }
    dd=(double)((xmax-xmin)/10);
    if((independentx)&(q.getparam(0)==515)){
        qr=choosetick(dd,60,4,8,4,5);
    }else{
        qr=choosetick(dd,10,4,40,2,2);
    }
    m1=qr*(Math.floor(xmin/qr));
    g.setColor(Color.black);
    if((!independentx))g.setColor(colors[colours[wl]]);
    while((float)m1<xmax){
        d=convert((float)m1,0);
        g.drawLine(d.width,ploty-5,d.width,ploty);        
        m1+=qr;
    }
    g.setColor(Color.black);
    if((!independentx))g.setColor(colors[colours[wl]]);
    g.drawLine(scalewidth,ploty-1,scalewidth+plotx,ploty-1);
    g.setColor(Color.black);
    if((independentx))g.setColor(colors[colours[wl]]);
    g.drawLine(scalewidth,0,scalewidth,ploty);
   
   }

   /**
    * Draw the lines
    * 
    * @param g Graphics context
    * @param xx X coordinates
    * @param yy Y coordinates
    * @param i Index
    */
   public void drawPoints(Graphics g,float[]xx,float[] yy,int i){
    g.setClip(scalewidth,0,plotx,ploty);
    g.setColor(colors[colours[i-1]]);
    if((q.delt!=0)&&(i==1)&&scrollingtime){
        xmax=xmax+q.delt;
        xmin=xmin+q.delt;
        q.delt=0;
    }
    if(independentx){
        float[] yq=new float[yy.length];
        for(int ii=0;ii<yy.length;ii++){
            yq[ii]=(yy[ii]*ym[i-1])+yc[i-1];
        }
        drawSome(g,xx,yq,symbols[i-1]);
    }else{
        float[] yq=new float[xx.length];
        for(int ii=0;ii<xx.length;ii++){
            yq[ii]=(yy[ii]*ym[i-1])+yc[i-1];
        }
        drawSome(g,yq,xx,symbols[i-1]);
    }
   }


   /**
    * Calcualte where to put tick marks
    * 
    * @param min Minimum
    * @param max Maximum
    * @return Vector of tick marks
    */
   
   public Vector getTickmarks(float min,float max){
    double d=max-min;
    Vector ans=new Vector();
    double qr=choosetick(d,10,4,40,2,2);
    double m1=qr*(Math.floor(min/qr));
    while((float)m1<max){
        ans.addElement(new tickmark((float)m1,false));
        m1+=qr;
    }
    return ans;
   }

   /**
    * Calculate where to put time marks
    * 
    * @param min Minimum time
    * @param max Maximum time
    * @return Vector of tick marks
    */
   public Vector getTimemarks(float min,float max){
    double d=max-min;
    if(d>2){
    Vector ans=new Vector();
    double qr=choosetick(d,60,4,8,4,5);
    double m1=qr*(Math.floor(min/qr));
    while((float)m1<max){
        ans.addElement(new tickmark((float)m1,true));
        m1+=qr;
    }
    return ans;
    }else{
        return getTickmarks(min,max);
    }
   }

   /**
    * Choose tick spacing
    * 
    * @param d Difference
    * @param base Base
    * @param min Minimum
    * @param max Maximum
    * @param x1 X1
    * @param x2 X2
    * @return Tick spacing
    */

   private double choosetick(double d,double base,double min,double max,double x1,double x2){
    double dq=Math.log(d);
    dq=dq/Math.log(base);
    double qr=Math.floor(dq);
    qr=Math.pow(base,(int)qr);
    if((d/qr)<min)qr/=x1;
    if((d/qr)>max)qr*=x2;
    return qr;
   }
	//{{DECLARE_CONTROLS
	//}}
	
	public void print(Graphics g){
	    if(checkscroll!=null)checkscroll.setVisible(false);
        int dx=5000/pq.fact;
        int dy=(pq.top*10+300)/pq.fact;
        int n=q.maxs.length;
	    for(int i=0;i<n;i++){
            whichone[i].setBounds(dx,dy,600/pq.fact,150/pq.fact);
            dy+=150/pq.fact;
        }
        movewhichone=true;
	    super.print(g);
	}
	public void paint(Graphics g){
	    if(checkscroll!=null)checkscroll.setVisible(true);
	    if(movewhichone){
          int dx=5000/pq.norm;
          int dy=(pq.top*10+300)/pq.norm;
          int n=q.maxs.length;
	      for(int i=0;i<n;i++){
              whichone[i].setBounds(dx,dy,600/pq.norm,150/pq.norm);
              dy+=150/pq.norm;
          }
          movewhichone=false;
        }
	    super.paint(g);
	}
	
	public void itemStateChanged(java.awt.event.ItemEvent event){
	  if(checkscroll!=null){
             scrollingtime=checkscroll.getState();
	     alwaysredraw=scrollingtime;
          }
      for(int i=0;i<whichone.length;i++){
          if(whichone[i].equals(which.getSelectedCheckbox())){
            whichline=i;
            if(i!=0){
              for(int ii=0;ii<=maxzooms;ii++){
                mhistory[0][ii]=ym[whichline-1];
                mhistory[1][ii]=yc[whichline-1];
              }
              mhist[0]=xmax;
              mhist[1]=xmin;
              mhist[2]=ymax;
              mhist[3]=ymin;
              mhistpos=historypos;
            }
            
          }
      }
      repaint();
	}
	
	public void changezooms(){
	    changedzoom=true;
	    if(whichline!=0){
            if(historypos>mhistpos){
              if(independentx){
              xmax=history[0][historypos];
              xmin=history[1][historypos];
              ymax=(ymax-yc[whichline-1])/ym[whichline-1];
              ymin=(ymin-yc[whichline-1])/ym[whichline-1];
              float dy=(ymax-ymin);
              float ody=(mhist[2]-mhist[3]);
              mhistory[0][mhistpos]=ym[whichline-1];
              mhistory[1][mhistpos]=yc[whichline-1];             
              ym[whichline-1]=(ody/dy);
              yc[whichline-1]=mhist[3]-ym[whichline-1]*ymin;
              ymax=mhist[2];
              ymin=mhist[3];
              }else{
              ymax=history[2][historypos];
              ymin=history[3][historypos];
              xmax=(xmax-yc[whichline-1])/ym[whichline-1];
              xmin=(xmin-yc[whichline-1])/ym[whichline-1];
              float dx=(xmax-xmin);
              float odx=(mhist[0]-mhist[1]);
              mhistory[0][mhistpos]=ym[whichline-1];
              mhistory[1][mhistpos]=yc[whichline-1];             
              ym[whichline-1]=(odx/dx);
              yc[whichline-1]=mhist[1]-ym[whichline-1]*xmin;
              xmax=mhist[0];
              xmin=mhist[1];
              }
              mhistpos=historypos;
            }else{
              if(mhistpos==0){
                ym[whichline-1]=1;
                yc[whichline-1]=0;
              }else{
	            mhistpos=historypos;	          
                ym[whichline-1]=mhistory[0][mhistpos];
                yc[whichline-1]=mhistory[1][mhistpos];
              }
              mhist[0]=xmax;
              mhist[1]=xmin;
              mhist[2]=ymax;
              mhist[3]=ymin;
            }
	        
	        
	        
	    }
	    
	}
}
