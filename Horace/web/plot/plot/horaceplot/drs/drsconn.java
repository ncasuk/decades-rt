package horaceplot.drs;

import java.io.*;
/**
 *
 * Connection to horace specifically for raw drs data.
 * 
 * @author dave.tiddeman
 */

public class drsconn extends horaceplot.horconn implements Runnable
{
    public char cont;
    Thread drsthread;
    public short[] freq;
    public int[] paras;
    public int[] index;
    public int[] times;
    public int np;
    private int maxfreq;
    public int ind;
    public int pind;
    public short[][] data;
    public int datalength=300;
    
    drsapp ap;
/**
 * Creates connection
 * @param app Applet called from
 * @throws IOException Failed to connect
 */
        public drsconn(drsapp app) throws IOException
        {
                super(app);
                ap=app;
        }
	/**
	 * Creates connection
	 * @param hos host name
	 * @param por port number
	 * @throws IOException Failed to connect
	 */
        public drsconn(String hos, int por) throws IOException
        {
                super(hos, por);
        }
        /**
         * Default creator
         *
         */
        public drsconn()
        {
            int a=1;
        }
        /**
         * 
         * @param params
         * @return if successful
         */
        public boolean setarrays(int[] params){
          np=0;
          times=new int[datalength];
          for(int i=0;i<params.length;i++)if(params[i]>0)np++;
          if(np>0){
            paras=new int[np];
            freq=new short[np];             
            index=new int[np];
            int inp=0;
            for(int i=0;i<params.length;i++)if(params[i]>0){
                paras[inp]=params[i];
                inp++;
            }
          }
          return (np!=0);
        }
        /**
         * 
         * @param js
         */
        public void readfreq(jscriptstream js){
                ind=0;
                maxfreq=0;
                try{
                for(int i=0;i<np;i++){
                  index[i]=ind;
                  if(js==null){
                    freq[i]=readshort();
                  }else{
                    freq[i]=js.readshort();
                    System.out.println("Freq="+freq[i]);
                  }
                  if(freq[i]>maxfreq)maxfreq=freq[i];
                  ind+=freq[i];
                }
                System.out.println("Ind="+ind);
                data=new short[ind][datalength];
                }catch(Exception e){}
        }
          /**
           * 
           *
           */
        public void stop(){
            cont='Q';
            try{Thread.sleep(100);}catch(Exception e){}
            close();
        }
            
        /**
         * 
         * @param params
         * @throws Exception
         */
        public void startit(int[] params) throws Exception{
               if(!setarrays(params)) throw new Exception("No valid parameters");
                drsthread=new Thread(this);
                writeString("DRS ");
                writeint(np);
                for(int i=0;i<np;i++){
                  writeint(paras[i]);
                }
                flush();
                readfreq(null);
                cont='C';
                pind=0;
                drsthread.start();
        }
                
           /**
            * 
            */     
        public void run(){
            try{
                String blank="                                                                ";
                StringBuffer[] sb=new StringBuffer[maxfreq];
                for(int i=0;i<maxfreq;i++)sb[i]=new StringBuffer(blank);
            while(cont=='C'){
                    times[pind]=readint();
                    inserttime(sb[maxfreq-1],times[pind]);
                    int pi=0;
                    for(int i=0;i<np;i++){                  
                       for(int ii=0;ii<freq[i];ii++){
                         data[pi][pind]=readshort();
                         
//        insertnumber(sb[maxfreq-ii-1],ival(pi,pind,(int)16),i*7+9);
                         insertnumber(sb[ii],data[pi][pind],i*7+9);
                         pi++;
                         
                       }
                    }
                    
                pind++;
                if(pind>=datalength)pind=0;
                    ap.writetext(sb,maxfreq);
                    ap.repaint();
                    writechar(cont);
                  }
                  writechar(cont);
                  close();
                }catch(Exception e){}
            
        }
        /**
         * 
         * @param i
         * @param nbits
         * @return i bit masked to nbits
         */
        public int ival(short i,int nbits){
            int iv=(2<<(nbits-1))-1;
            return (i&iv);
        }
        /**
         * 
         * @param i
         * @param i2
         * @param nbits
         * @return bit masked data value data[i,i2]
         */
        public int ival(int i,int i2,int nbits){
            int iv=ival(data[i][i2],nbits);
            return iv;
        }
        /**
         * 
         * @param sb
         * @param number
         * @param position
         */
        public void insertnumber(StringBuffer sb,int number,int position){
            StringBuffer s=new StringBuffer();
            s.append(number);
            while(s.length()<5)s.insert(0," ");
            for(int i=0;i<5;i++){
                sb.setCharAt(i+position,s.charAt(i));
            }
        }
        /**
         * 
         * @param sb
         * @param tm
         */
        public void inserttime(StringBuffer sb,int tm){
            StringBuffer s=new StringBuffer();
            s.append(horaceplot.stringutl.gmt(tm));
            for(int i=0;i<s.length();i++){
                sb.setCharAt(i,s.charAt(i));
            }
        }
        
}
