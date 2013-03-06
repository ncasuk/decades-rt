

package horaceplot.plot;

import java.lang.Math;
import java.awt.*;
/**
 * Creates a hodograph plot
 * 
 * @author Dave tiddeman (dave.tiddeman@metoffice.gov.uk)
 * @version 1
 */

public class hodoplot extends zoomplot
{
     /**
     * Horace connection
     */
     

    /**
     * Creates a new hodograph
     */
    public hodoplot(){
        super(100,-100,100,-100);
    
		//{{INIT_CONTROLS
		//}}
	}
    /**
     * Creates a new hodograph
     */
    /**
     * Initializes the hodograph
     */
    
    public void init(){
     setBackground(Color.white);
//        q.start(getparams(),this,1,1000);
        setDatamaxmin(100,-100,100,-100);
        setmaxmin(100,-100,100,-100);
    
   }

    /**
     * Draws the circular background with wind strengths.
     * 
     * @param g Graphics context
     */
   
    public void drawBackground(Graphics g){
       g.setPaintMode();
       g.setColor(Color.black);
       g.setFont(new Font("SansSerif",Font.BOLD,fontSize));       StringBuffer sb;
       float z=0;
       for(int w=0;w<=100;w+=10){
         sb=new StringBuffer("").append(w);
         drawString(g,z,z,sb.toString());
         z+=7.07;
         drawCircle(g,(float)w);
       }
       float[] xx={0,0};
       float[] yy={-100,100};
       drawPolyline(g,xx,yy);
       yy[0]=0;
       yy[1]=0;
       xx[0]=-100;
       xx[1]=100;
       drawPolyline(g,xx,yy);
       xx=polartocart(180,90);
       drawString(g,xx[0],xx[1],"N");
       xx=polartocart(270,90);
       drawString(g,xx[0],xx[1],"E");
       xx=polartocart(0,90);
       drawString(g,xx[0],xx[1],"S");
       xx=polartocart(90,90);
       drawString(g,xx[0],xx[1],"W");
    }

   /**
    * Converts polar to cartesian coordinates
    * 
    * @param angle Angle in degrees
    * @param radius Radius
    * @return Dimension of X and Y coordinates
    */
   
   public float[] polartocart(float angle, float radius){
      float[] ans=new float[2];
      double ang=((double)angle/180.0)*Math.PI;
      ans[0]=-radius*((float)Math.sin(ang));
      ans[1]=-radius*((float)Math.cos(ang));
      return ans;
   }

   /**
    * Converts polar to cartesian coordinates, for arrays.
    * 
    * @param angle Angles in degrees
    * @param strength Wind strengths
    * @return X and Y coordinates in Array
    */
   
   public float[][] polartocart(float[] angle,float[] strength){
      int n=angle.length;
      float[][] ans=new float[2][n];
      for(int i=0;i<n;i++){
        float[] ans1=polartocart(angle[i],strength[i]);
        ans[0][i]=ans1[0];
        ans[1][i]=ans1[1];
      }
      return ans;
        
   }   

   /**
    * Draws wind strengths onto background
    * 
    * @param g Graphics context
    * @param angle Array of angles
    * @param strength Array of wind strengths
    * @param i Number of points
    */
    
   public void drawPoints(Graphics g,float[] angle,float[] strength,int i){
        g.setColor(colors[colours[i-1]]);
       float[][] cart=polartocart(angle,strength);
       drawSome(g,cart[0],cart[1],symbols[i-1]);
   }

	//{{DECLARE_CONTROLS
	//}}
} 
