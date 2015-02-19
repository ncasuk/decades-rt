
package horaceplot.plot;

import java.awt.*;
import java.io.*;
import java.awt.event.*;
/**
 * Absract class for creating various zoomable plots.
 * 
 * @author Dave tiddeman (dave.tiddeman@metoffice.gov.uk)
 * @version 1
 * @version 1.1 Removes NaN values from plotting via convert method
 */

public abstract class zoomplot extends java.awt.Canvas   
{
         /**
		 * Maximum number of zooms = 20
		 */
		public int maxzooms=20;
          zmse azmse = new zmse();
          zkey azkey = new zkey();
          zmse2 bzmse = new zmse2();
          boolean isdragging=false;
          public float ymax,ymin,xmax,xmin;
          public float[][] history=new float[4][maxzooms+1];
          public int historypos=0;
          public int y1,y2,x1,x2;
          public int h,w,hoff,woff;
          public int plotdatamarker=-1;
          float delt=0;
          public static int[][][] shape=
             {{{-1,-1},{-1,1},{1,1},{1,-1}},
               {{0,1},{1,0},{0,-1},{-1,0}}};
          public final int LINE=-1;
          public final int SQUARE=3;
          public final int DIAMOND=4;
          public final int DOT=0;
          public final int CROSS=1;
          public final int X=2;
          public Color[] colors={Color.white,Color.black,
   Color.red,Color.green,Color.blue,Color.cyan,Color.orange,
   Color.magenta,Color.pink,Color.yellow,Color.gray};
//         public Vector data=new Vector();
         public plotconn q; 
         public int fontSize=10;
         public int[] symbols,colours;
         public boolean changedzoom=false;
         public boolean alwaysredraw=false;
         Image offScreenBuffer;
          /**
           * Default zoomable canvas
           * zoomplot(100,0,100,0)
           */
          public zoomplot(){
        this((float)100.0,(float)0,(float)100.0,(float)0);
          }
          

          /**
           * Zoomable canvas supplying the max and min.
           * 
           * @param xmx X Maximum
           * @param xmn X minimum
           * @param ymx Y Maximum
           * @param ymn Y Minimum
           */
          
          public zoomplot(float xmx,float xmn,float ymx,float ymn){
            ymax=ymx;
            ymin=ymn;
            xmax=xmx;
            xmin=xmn;
            setDatamaxmin(xmx,xmn,ymx,ymn);
            Dimension d=getSize();
            w=d.width;
            h=d.height;
            woff=0;
            hoff=0;
            addMouseListener(azmse);
            addKeyListener(azkey);
        }

        /**
         * Overrides Canvas.setBounds , sets plotting size equal to the boundary.
         * 
         * @param ix X
         * @param iy Y
         * @param iwidth Width
         * @param iheight Height
         */
        
        public void setBounds(int ix,int iy,int iwidth,int iheight){
            synchronized(this){
             w=iwidth;
             h=iheight;
             woff=0;
             hoff=0;
             fontSize=iwidth/40;
             super.setBounds(ix,iy,iwidth,iheight);
            }
    }

    /**
     * Sets a plotting area within the canvas
     * 
     * @param ix X
     * @param iy Y
     * @param iwidth Width
     * @param iheight Height
     */
    
    public void setPlotBounds(int ix,int iy,int iwidth,int iheight){
      synchronized(this){
        w=iwidth;
        h=iheight;
        woff=ix;
        hoff=iy;
        fontSize=iwidth/40;
      }
    }

         /**
          * Sets the maximum size to zoom out to.
          * 
          * @param xmx X Maximum
          * @param xmn X Minimum
          * @param ymx Y Maximum
          * @param ymn Y Minimum
          */
            
         public void setDatamaxmin(float xmx,float xmn,float ymx,float ymn){
            history[0][0]=xmx;
            history[1][0]=xmn;
            history[2][0]=ymx;
            history[3][0]=ymn;
         }

         /**
          * Sets the current zoomed size.
          * 
          * @param xmx X Maximum
          * @param xmn X Minimum
          * @param ymx Y Maximum
          * @param ymn Y Mininmum
          */
         public void setmaxmin(float xmx,float xmn,float ymx,float ymn){
            xmax=xmx;
            xmin=xmn;
            ymax=ymx;
            ymin=ymn;
         }
         public float getDataxmax(){
            return history[0][0];
         }
         public float getDataxmin(){
            return history[1][0];
         }
         public float getDataymax(){
            return history[2][0];
         }
         public float getDataymin(){
            return history[3][0];
         }
          

    public void drawPolygon(Graphics g,float[] x,float[] y){
        int n=x.length;
            drawPolygon(g,x,y,n);
        }
     public void drawPolygon(Graphics g,float[] x,float[] y,int n){
        int[] xx,yy;
        xx=new int[n];
        yy=new int[n];
        n=convert(x,y,xx,yy,n);
        g.setPaintMode();
        g.fillPolygon(xx,yy,n);
    }
   
    
    public void drawPolyline(Graphics g,float[] x,float[] y){
        int n=x.length;
            drawPolyline(g,x,y,n);
        }
    public void drawPolyline(Graphics g,float[] x,float[] y,int n){
       drawSome(g,x,y,LINE,n);
    }
    
    public void drawLine(Graphics g,float x1,float y1,float x2,float y2){
    	try{
        Dimension d1=convert(x1,y1);
        Dimension d2=convert(x2,y2);
        g.setPaintMode();
//        g.drawLine(d1.width,d1.height,d2.width,d2.height);
        Clipping.drawLine(g,d1.width,d1.height,d2.width,d2.height,g.getClipBounds());
    	}catch (IllegalArgumentException iae){
    		
    	}
    }
    
    public void drawDashedline(Graphics g,float[] x,float[] y,int dashes,int gaps){
        int[] xx,yy;
        xx=new int[2];
        yy=new int[2];
        if(x.length>=2){
          g.setPaintMode();
          if(convert(x,y,xx,yy,2)==2){
          float x2=xx[1];
          float y2=yy[1];
          float x1=xx[0];
          float y1=yy[0];
          float dist=(float)Math.sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1));
          float n=(dist/(float)(dashes+gaps));
          float fact1=((float)dashes/(float)(dashes+gaps));
          float dx=(x2-x1)/n;
          float dy=(y2-y1)/n;
          float dxn=dx*fact1;
          float dyn=dy*fact1;          
          while(((dx<0)&(x1>x2))|((dx>0)&(x1<x2))
               &((dy<0)&(y1>y2))|((dy>0)&(y1<y2))){
            if((x1>woff)&(x1<w)&(y1>hoff)&(y1<h)){
             float x12=x1+dxn;
             float y12=y1+dyn;
             if((dx<0)&(x12<x2)){x12=x2;}
             if((dy<0)&(y12<y2)){y12=y2;}
             if((dx>0)&(x12>x2)){x12=x2;}
             if((dy>0)&(y12>y2)){y12=y2;}           
            Clipping.drawLine(g,(int)x1,(int)y1,(int)(x12),(int)(y12),g.getClipBounds());
            }
            x1+=dx;
            y1+=dy;
          }
          }
        }
    }
          
          
    public void drawCircle(Graphics g,float x,float y,float r){
    	try{
        float topx=(x-r);
        float topy=(y+r);
        float botx=(x+r);
        float boty=(y-r);
        Dimension d1=convert(topx,topy);
        Dimension d2=convert(botx,boty);
        d2.width-=d1.width;
        d2.height-=d1.height;
        g.setPaintMode();
        g.drawOval(d1.width,d1.height,d2.width,d2.height);
    	}catch (IllegalArgumentException iae){
    		
    	}

    }
    
    public void drawCircle(Graphics g,float r){
        drawCircle(g,0,0,r);
    }
    
    public void drawSome(Graphics g,float[] x,float[] y,int style){
        int n=y.length;
        drawSome(g,x,y,style,n);
    }

    public void drawSome(Graphics g,int[] x,int[] y,int style){
        int n=y.length;
        drawSome(g,x,y,style,n);
    }
    
    public void drawSome(Graphics g,float[] x,float[] y,int style,int n){
        int[] xx,yy;
        xx=new int[n];
        yy=new int[n];
        n=convert(x,y,xx,yy,n);
        drawSome(g,xx,yy,style,n);
    }
        
    public void drawSome(Graphics g,int[] xx,int[] yy,int style,int n){
//      g.setPaintMode();
      if(style==LINE){
        if(n>0){
        	Rectangle r=g.getClipBounds();
        	if(r!=null){        	
            Clipping.drawLine(g,xx[0],yy[0],xx[0],yy[0],r);
            Clipping.drawPolyline(g,xx,yy,n,r);
        	}
        }
      }else{
        for(int i=0;i<n;i++){
          drawShape(g,xx[i],yy[i],style);
        }
      }
    }
        
        
    public void drawPoint(Graphics g,float x,float y,int style){
    	try{
        Dimension d=convert(x,y);
 //       Graphics g=getGraphics();
        g.setPaintMode();
        drawShape(g,d.width,d.height,style);
    	}catch (IllegalArgumentException iae){
        	
        }
    }

    public void drawShape(Graphics g,int x,int y,int style){
       int size=2;
       switch(style){
       case DOT:Clipping.drawLine(g,x,y,x,y,g.getClipBounds());break;
       case CROSS:{Clipping.drawLine(g,x,y-size,x,y+size,g.getClipBounds());
               Clipping.drawLine(g,x+size,y,x-size,y,g.getClipBounds());break;}
       case X:{Clipping.drawLine(g,x-size,y-size,x+size,y+size,g.getClipBounds());
               Clipping.drawLine(g,x+size,y-size,x-size,y+size,g.getClipBounds());break;}
       default:{
               int np=shape[0].length;
               int[] xx=new int[np];
               int[] yy=new int[np];
               for(int i=0;i<np;i++){
                  xx[i]=x+shape[style-3][i][0]*size;
                  yy[i]=y+shape[style-3][i][1]*size;
               }
               g.drawPolygon(xx,yy,np);
               }
       }
    }   
        
    public void drawString(Graphics g,float x,float y,String s){
    try{
        Dimension d=convert(x,y);
        g.setPaintMode();
        g.drawString(s,d.width,d.height);
	}catch (IllegalArgumentException iae){
		
	}
    }
    
    
    public int convert(float[] x,float[] y,int[] xx,int[] yy,int n){
        double dx=((float)w)/(xmax-xmin);
        double dy=((float)h)/(ymax-ymin);
        double x1,y1;
        int nn=0;
        for(int i=0;i<n;i++){
        	if((x[i]==x[i])&&(y[i]==y[i])){
            x1=(dx*(double)(x[i]-xmin));
            y1=(dy*(double)(ymax-y[i]));
            xx[nn]=(int)x1+woff;
            yy[nn]=(int)y1+hoff;
            nn++;
        	}
        }
        return nn;
    }
    public Dimension convert(float x,float y) throws IllegalArgumentException{
    	if((x!=x)||(y!=y)){
    		throw new IllegalArgumentException("NaN");
    	}else{
        double dx=((double)w)/(double)(xmax-xmin);
        double dy=((double)h)/(double)(ymax-ymin);
        int xx=(int)(dx*(x-xmin))+woff;
        int yy=(int)(dy*(ymax-y))+hoff;
        Dimension d=new Dimension(xx,yy);
        return d;
    	}
    }
    
    public String getunits(int n){
          String p=((dataset)q.data.elementAt(n)).units;
          return p;
    }
    
    public String getname(int n){
          String p=((dataset)q.data.elementAt(n)).name;
          return p;
    }
    
    
    public void print(Graphics g){
        synchronized(this){
        drawBackground(g);
        drawData(g);
        }
    }

    public void paint(Graphics g){
        synchronized(this){
        drawBackground(g);
        drawData(g);
        }
        if(changedzoom){
       plotapp pa=(plotapp)getParent();
       pa.changezooms();
        changedzoom=false;
        }
       
       if(isdragging){  g.setColor(Color.black);
                        g.setXORMode(Color.white);
                        int xa=Math.max(x1,x2);
                        int xb=Math.min(x1,x2);
                        int ya=Math.max(y1,y2);
                        int yb=Math.min(y1,y2);
                        g.drawRect(xb,yb,xa-xb,ya-yb);
       }
    }
    
    public void drawData(Graphics g){
        int n=q.data.size();
        dataset d=(dataset)q.data.elementAt(0);
        float[] qx=d.getdata();
		g.setClip(0,0,getSize().width,getSize().height);
        for(int i=1;i<n;i++){
            d=(dataset)q.data.elementAt(i);
            drawPoints(g,qx,d.getdata(),i);
        }
        plotdatamarker=d.datalength-1;
    }
    public void drawlatestData(){
        Graphics g=getGraphics();
        if((g!=null)&&(q.delt!=0)){
         if(alwaysredraw){
            repaint();
         }else{
          if(plotdatamarker>=0){
           int n=q.data.size();
           dataset d=(dataset)q.data.elementAt(0);
           int pdm=d.datalength-1;
           float[] qx=d.getdata(plotdatamarker);
           if(qx.length>1){
			g.setClip(0,0,getSize().width,getSize().height);
            for(int i=1;i<n;i++){
             d=(dataset)q.data.elementAt(i);
             drawPoints(g,qx,d.getdata(plotdatamarker),i);
            }
            plotdatamarker=pdm;
           }
          }
         }
        }
    }
    public void initialize(plotapp plotapp,int start,int stop,int st,int[] ps,String[] pn,String[] pu,int[] co,int[] sy){
        try{
        q=new plotconn(plotapp);
        }catch(IOException ioe){
          System.out.println(ioe);
        }
        if(ps!=null){
          for(int i=0;i<ps.length;i++){
           q.addPara(ps[i],pn[i],pu[i]);
          }
        }
        colours=co;
        symbols=sy;
        q.start(plotapp,start,stop,st);
        init();
   }

    public abstract void drawBackground(Graphics g);
    
    public abstract void drawPoints(Graphics g,float[] xx,float[] yy,int i);
    
    public abstract void init();
    
    public void changezooms(){
                  changedzoom=true;
        }
    
   /**
    * Stop collecting more data to plot.
    */
   public void stop(){
        q.stop();
   }

        class zmse extends java.awt.event.MouseAdapter
        {
                public void mouseClicked(java.awt.event.MouseEvent event){
                    if(event.getModifiers()!=InputEvent.BUTTON1_MASK){
                       xmax=history[0][historypos];
                       xmin=history[1][historypos];
                       ymax=history[2][historypos];
                       ymin=history[3][historypos];
                       historypos--;
                        if(historypos<0){
                            historypos=0;
                            }
                        changezooms();
                        repaint();
                    }else{
                        repaint();
                    }
                }
                public void mousePressed(java.awt.event.MouseEvent event){
                    if(event.getModifiers()==InputEvent.BUTTON1_MASK){
                             y1=event.getY();
                             x1=event.getX();
                             if((x1>woff)&(x1<(w+woff))&(y1>hoff)&(y1<(h+hoff))){
                               y2=y1;
                               x2=x1;
                               isdragging=true;
                       addMouseMotionListener(bzmse);
                     }
                        }
                }
                public void mouseReleased(java.awt.event.MouseEvent event){
                    if((event.getModifiers()==InputEvent.BUTTON1_MASK)&&isdragging){
                        isdragging=false;
                        removeMouseMotionListener(bzmse);
                        Graphics g=getGraphics();
                        g.setXORMode(Color.white);
                        int xa=Math.max(x1,x2);
                        int xb=Math.min(x1,x2);
                        int ya=Math.max(y1,y2);
                        int yb=Math.min(y1,y2);
                        g.drawRect(xb,yb,xa-xb,ya-yb);
                            if((xa!=xb)&(ya!=yb)){
                      float dx=xmax-xmin;
                      float dy=ymax-ymin;
                      float xfa=(float)(xa-woff)/(float)(w);
                      float xfb=(float)(xb-woff)/(float)(w);
                      float yfb=1-((float)(ya-hoff)/(float)(h));
                      float yfa=1-((float)(yb-hoff)/(float)(h));
                      historypos++;
                      if(historypos>maxzooms){historypos=maxzooms;}
                      history[0][historypos]=xmax;
                      history[1][historypos]=xmin;
                      history[2][historypos]=ymax;
                      history[3][historypos]=ymin;
                      xmax=xfa*dx+xmin;
                      xmin=xfb*dx+xmin;
                      ymax=yfa*dy+ymin;
                      ymin=yfb*dy+ymin;
                      changezooms();
                      repaint();
                        }
                }
        }
                        
                    
        }
        
        class zmse2 extends java.awt.event.MouseMotionAdapter{
                public void mouseDragged(java.awt.event.MouseEvent event){
     if(isdragging){
        Graphics g=getGraphics();
        g.setXORMode(Color.white);
        int xa=Math.max(x1,x2);
        int xb=Math.min(x1,x2);
        int ya=Math.max(y1,y2);
        int yb=Math.min(y1,y2);
        g.drawRect(xb,yb,xa-xb,ya-yb);
        y2=event.getY();
        x2=event.getX();
        if(x2>=w+woff)x2=w+woff-1;
        if(y2>=h+hoff)y2=h+hoff-1;
        if(x2<woff)x2=woff;
        if(y2<hoff)y2=hoff;
        xa=Math.max(x1,x2);
        xb=Math.min(x1,x2);
        ya=Math.max(y1,y2);
        yb=Math.min(y1,y2);
        g.drawRect(xb,yb,xa-xb,ya-yb);
                    }
                }
        }
     
        class zkey extends java.awt.event.KeyAdapter{
            
          public void keyPressed(java.awt.event.KeyEvent event){
            float dx=0;
            float dy=0;
            
		if(event.getKeyCode()==KeyEvent.VK_UP)dy=1;
		if(event.getKeyCode()==KeyEvent.VK_DOWN)dy=-1;
		if(event.getKeyCode()==KeyEvent.VK_LEFT)dx=-1;
		if(event.getKeyCode()==KeyEvent.VK_RIGHT)dx=1;
		if((dx!=0)||(dy!=0)){
          dx=dx*(xmax-xmin)/10;
          dy=dy*(ymax-ymin)/10;
          float max1=xmax+dx;
          float min1=xmin+dx;
          if((max1<history[0][0])&&(max1>history[1][0])&&
             (min1<history[0][0])&&(min1>history[1][0])){
                xmax=max1;
                xmin=min1;
             }
          max1=ymax+dy;
          min1=ymin+dy;
          if((max1<history[2][0])&&(max1>history[3][0])&&
             (min1<history[2][0])&&(min1>history[3][0])){
                ymax=max1;
                ymin=min1;
             }
          changezooms();
          repaint();
        }
            
          }
            
            
            
        }
}
