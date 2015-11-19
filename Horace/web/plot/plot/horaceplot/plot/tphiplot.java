

package horaceplot.plot;

import java.lang.Math;
import java.awt.*;
/**
 * Draws a tephigram, temperature, entropy plot.
 * 
 * @author Dave tiddeman (dave.tiddeman@metoffice.gov.uk)
 * @version 1
 */

public class tphiplot extends zoomplot

{

    /**
     * Horace connection
     */
 
    /**
     * Constant
     */
    
    private final double L=2.5e6;

    /**
     * Constant
     */
    private final double RV=461.0;

    /**
     * Constant
     */
    private final double RD=287.0;

    /**
     * Constant
     */
    private final double CP=1.01e3;

    /**
     * Constant
     */
    private final double KELV=273.15;

    /**
     * Constant
     */
    private final double K=0.286;

    /**
     * Constant
     */
    private final double MA=300.0;

    /**
     * Constant
     */
    private final double[] MIXLINES={0.001,0.002,0.005,0.01,0.02,
    0.15,0.2,0.3,0.4,0.5,0.6,0.8,1.0,1.5,2.0,2.5,3.0,4.0,
    5.0,6.0,7.0,8.0, 9.0, 10.0, 12.0, 14.0, 16.0, 18.0, 20.0,    
     24.0, 28.0, 32.0, 36.0, 40.0, 44.0, 48.0, 52.0, 56.0, 60.0, 68.0, 
     80.0};
     private double TempLine=10.0;
     private double SALRLine=1.0;
     private double ThetaLine=10.0;
     private double PressLine=50.0;
     private final char ctheta=952;
     private final char cdegree=176;

    /**
     * Creates a new tephigram, with set maximums and minimums
     * 
     * @param xmx Maximum X
     * @param xmn Minimum X
     * @param ymx Maximum Y
     * @param ymn Minimum Y
     */
     
    
    public tphiplot(float xmx,float xmn,float ymx,float ymn){
        super(xmx,xmn,ymx,ymn);
        setmaxmin(xmx,xmn,ymx,ymn);
    
		//{{INIT_CONTROLS
		setSize(0,0);
		//}}
	}

    /**
     * Creates a new tephigram
     */
    public tphiplot(){
        this(1744,1649,1803,1693);
    }

   /**
    * Creates a new tephigram
    * 
    * @param ps
    * @param pn
    * @param pu
    * @param co
    * @param sy
    */


    /**
     * Initialize plot and start reading data
     */
    
    public void init(){
        setBackground(Color.white);
        setmaxmin(1800,1600,1800,1673);
        setDatamaxmin(1800,1600,1800,1673);

    }

    /**
     * Set gap between temperature lines, in degrees.
     * 
     * @param T Spacing in degrees.
     */

    public void setTempLine(double T){
        TempLine=T;
    }

    /**
     * Set gap between saturated adiabtic lapse rate lines, in degrees.
     * 
     * @param T Spacing in degrees.
     */
    public void setSALRLine(double T){
        SALRLine=T;
    }

    /**
     * Set gap between potential temperature lines, in degrees.
     * 
     * @param T Spacing in degrees.
     */
    public void setThetaLine(double T){
        ThetaLine=T;
    }

    /**
     * Set gap between pressure lines, in degrees.
     * 
     * @param T Spacing in hectapascals.
     */
    public void setPressLine(double T){
        PressLine=T;
    }

    /**
     * Gradient of SALR line
     * 
     * @param p Pressue, hPa
     * @param t1 Temp, C
     * @param dp Pressure change, hPa
     * @param nostop no stop.
     * @return dT and dp
     */
    
    private float[] SALRgrad(double p,double t1,double dp, boolean nostop){
 
       float[] ans=new float[2];
       double t=t1+KELV;
       double lsbc=(L/RV)*((1.0/KELV)-(1.0/t));
       double rw=6.11*Math.exp(lsbc)*(0.622/p);
       double lrwbt=(L*rw)/(RD*t);
       double nume=((RD*t)/(CP*p))*(1.0+lrwbt);
       double deno=1.0+(lrwbt*((0.622*L)/(CP*t)));
       double gradi=nume/deno;
       double dt=(dp*gradi);
       if (((t1+dt)<(-50.0))&(!nostop)){
           dt=-50.0-t1;
           dp=dt/gradi;
       }
       ans[0]=(float)dp;
       ans[1]=(float)dt;
       return ans;
    }

    /**
     * Mass mixing ratio
     * 
     * @param p Pressure
     * @param mix Ratio
     * @return Temperatures
     */
    
    private double[] massmix(double[] p,double mix){
        int n=p.length;
        double[] t=new double[n];
        for(int i=0;i<n;i++){
          double vapp=p[i]*(8.0/5.0)*(mix/1000.0);
          t[i]=1.0/((1.0/KELV)-((RV/L)*Math.log(vapp/6.11)))-KELV;
        }
        return t;
    }

    /**
     * Converts X and Y coordinates to T,p and phi
     * 
     * @param x X
     * @param y Y
     * @return T and phi
     */
    
    public float[] xytphi(double x,double y){
        float[] ans=new float[3];
        double temp=(x-y)/2.0;
        double phi=(x-temp)/MA;
        double theta=Math.exp(phi);
        double t=temp+KELV;
        double press=1000.0/(Math.pow((theta/t),3.4965));
        theta=theta-KELV;
        ans[0]=(float)press;
        ans[1]=(float)temp;
        ans[2]=(float)theta;
        return ans;
    }

    /**
     * Converts temp and press coordinates to X and Y
     * 
     * @param press Pressure
     * @param temp Temperature
     */
    public float[] tphixy(double press,double temp){
        float[] ans=new float[2];
        if((press!=press)||(temp!=temp)){
        	ans[0]=Float.NaN;
        	ans[1]=Float.NaN;
        }else{
            double t=temp+KELV;
            double theta=t*(Math.pow((1000.0/press),K));
            double phi=Math.log(theta);
            double x=phi*MA+temp;
            double y=phi*MA-temp;
            ans[0]=(float)x;
            ans[1]=(float)y;
        }
        return ans;
    }

    /**
     * Converts temp and press coordinates to X and Y
     * 
     * @param press pressure
     * @param temp temperature
     */
    
    public float[][] tphixy(double[] press,double[] temp){
        float[][] ans=new float[2][press.length];
        float[] ans1=new float[2];
        for(int i=0;i<press.length;i++){
        ans1=tphixy(press[i],temp[i]);
        ans[0][i]=ans1[0];
        ans[1][i]=ans1[1];
        }
        return ans;
    }

    /**
     * Converts temp and press coordinates to X and Y
     * 
     * @param press pressure
     * @param temp temperature
     */
    
    public float[][] tphixy(float[] press,float[] temp){
        float[][] ans=new float[2][press.length];
        float[] ans1=new float[2];
        for(int i=0;i<press.length;i++){
        ans1=tphixy((double)press[i],(double)temp[i]);
        ans[0][i]=ans1[0];
        ans[1][i]=ans1[1];
        }
        return ans;
    }

    /**
     * Draw the tephigram background.
     * 
     * @param g Graphics context
     */
        
        
    public void drawBackground(Graphics g){
//
// ADD TEMPERATURE LINES and values
//;
//;
g.setPaintMode();
g.setColor(Color.green);
g.setFont(new Font("SansSerif",Font.BOLD,fontSize));
double mxx,mxy,mnx,mny,pr,te,theta,phm4;
float[] x,y,scratch2,scratch3;
float[][] scratch;
scratch2=new float[2];
scratch3=new float[3];
x=new float[2];
y=new float[2];
pr=te=theta=0;
mxx=getDataxmax();
mnx=getDataxmin();
mxy=getDataymax();
mny=getDataymin();
mxx=xmax;
if(xmin>mnx)mnx=xmin;
mxy=ymax;
if(ymin>mny)mny=ymin;
scratch3=xytphi((mxx+mnx)/2.0,(mxy+mny)/2.0);
pr=scratch3[0];
te=scratch3[1];
theta=scratch3[2];
theta=theta+2.0-Math.IEEEremainder(theta,2.0);
phm4=MA*(Math.log(theta+KELV));
double[] t1={-20.0,90.0};
double[] phm=new double[2];
phm[0]=MA*Math.log(t1[0]+KELV);
phm[1]=MA*Math.log(t1[1]+KELV);
for(double T=-80.0;T<=30.0;T+=TempLine){ 
  x[0]=(float)(phm[0]+T);
  y[0]=(float)(phm[0]-T);
  x[1]=(float)(phm[1]+T);
  y[1]=(float)(phm[1]-T);
  if (T==0.0){drawDashedline(g,x,y,5,5);} else {drawPolyline(g,x,y);}
  String sT=new StringBuffer("").append((int)T).append(cdegree).toString();
  drawString(g,(float)(phm4+T),(float)(phm4-T),sT);
}
//;
//; ADD THETA LINES
//;
scratch2=xytphi((mnx+mxx)/2.0,(mxy+mny)/2.0);
pr=scratch2[0];
te=Math.floor(scratch2[1]);
t1[0]=-80.0;
t1[1]=30.0;
for(double thet=-20.0;thet<=90.0;thet+=ThetaLine){
  double phm1=MA*Math.log(thet+KELV);
  x[0]=(float)(phm1+t1[0]);
  y[0]=(float)(phm1-t1[0]);
  x[1]=(float)(phm1+t1[1]);
  y[1]=(float)(phm1-t1[1]);
  drawPolyline(g,x,y);
  if(phm1!=phm4){
    String sT=new StringBuffer("").append((int)thet).append(cdegree).append(ctheta).toString();
    drawString(g,(float)(phm1+te),(float)(phm1-te),sT);
  }
}

 
//; PRESSURE LINES
float[] xx=new float[12];
float[] yy=new float[12];

for(double press=1100.0;press>=200.0;press-=PressLine){
  for(int i=0;i<12;i++){
    double temp=10.0*(double)i-80.0;
    scratch2=tphixy(press,temp);
    xx[i]=scratch2[0];
    yy[i]=scratch2[1];
  }
  drawPolyline(g,xx,yy);
}

//;
//; SALR LINES
//;

scratch2=xytphi(((mnx+mxx)/2),((mny+mxy)/2));
double prr=(double)scratch2[0];
prr=50.0*Math.floor(prr/50.0)-5.0;
double[] te200=new double[200];
double[] pr200=new double[200];
for (double temp=-40.0;temp<=60.0;temp+=SALRLine){
//;
//  ;Above 1000mb
//;
  te200[0]=temp;
  pr200[0]=1000.0;
  double dp=-5.0;
  for(int i=0;i<=198;i++){
    scratch2=SALRgrad(pr200[i],te200[i],dp,false);
    te200[i+1]=te200[i]+scratch2[1];
    pr200[i+1]=pr200[i]+scratch2[0];
 if (pr200[i+1]==prr) {
    if(Math.IEEEremainder(temp,5.0)==0){
    scratch2=tphixy(pr200[i+1],te200[i+1]);
    String sT=new StringBuffer("").append((int)temp).append(cdegree).append(ctheta).append("w").toString();
    drawString(g,scratch2[0],scratch2[1],sT);
    }
 }
 }

 scratch=new float[2][200];
 scratch=tphixy(pr200,te200);
 drawPolyline(g,scratch[0],scratch[1]);
//;
//  ;Below 1000mb
//;
double[] te50=new double[50];
double[] pr50=new double[50];
  te50[0]=temp;
  pr50[0]=1000.0;
  dp=5.0;
  for(int i=0;i<=48;i++){ 
    scratch2=SALRgrad(pr50[i],te50[i],dp,false);
    te50[i+1]=te50[i]+scratch2[1];
    pr50[i+1]=pr50[i]+scratch2[0];
  }
 scratch=new float[2][50];
 scratch=tphixy(pr50,te50);
 drawPolyline(g,scratch[0],scratch[1]);
}

//;
//  ; ADD PRESSURE FIGURES ON SALR LINE
//;
  scratch3=xytphi(((mnx+mnx+mxx)/3),mny);
  prr=50.0*Math.floor(scratch3[0]/50.0);
  double ter=2.0*Math.ceil(scratch3[1]/2.0);
  double dp=-5.0;
  scratch2=tphixy(prr,ter);
  double prhgt=44330.77*(1.-(Math.pow((prr/1013.25),0.19026)));
  String sT=new StringBuffer("").append((int)prr).append("mb (").append((int)prhgt).append("m)").toString();
  drawString(g,scratch2[0],scratch2[1],sT);
  for(int i=0;i<=230;i++){
    scratch2=SALRgrad(prr,ter,dp,true);
    ter=ter+scratch2[1];
    prr=prr+scratch2[0];
    if (Math.IEEEremainder(prr,50.0)==0.0){
      scratch2=tphixy(prr,ter);
      prhgt=44330.77*(1.-(Math.pow((prr/1013.25),0.19026)));
      sT=new StringBuffer("").append((int)prr).append("mb (").append((int)prhgt).append("m)").toString();
      drawString(g,scratch2[0],scratch2[1],sT);
    }
  }

//;
//; MASS MIX LINES
//;
  scratch3=xytphi(mnx,mny);
  prr=50.0*Math.floor(scratch3[0]/50.0)-5.0;
  double[] p={200.0,prr+100.,prr};
  xx=new float[3];
  yy=new float[3];
  double[] tt=new double[3];
  scratch=new float[2][3];
for(int ml=0;ml<41;ml++){
  tt=massmix(p,MIXLINES[ml]);
  scratch=tphixy(p,tt);
  drawDashedline(g,scratch[0],scratch[1],1,9);
  sT=new StringBuffer("").append((float)MIXLINES[ml]).toString();
  drawString(g,scratch[0][2],scratch[1][2],sT);
}

}

   /**
    * Draw the temperature and dewpoint on background
    * 
    * @param g Graphics context
    * @param press Pressures
    * @param temp Temperatures
    * @param i 1 for temperature 2 for dewpoint
    */


   public void drawPoints(Graphics g,float[] press,float[] temp,int i){
      float[][] xy=tphixy(press,temp);
      g.setColor(colors[colours[i-1]]);
      drawSome(g,xy[0],xy[1],symbols[i-1]);
   }


	//{{DECLARE_CONTROLS
	//}}
}
