import java.util.*;
import java.io.File;
import java.util.stream.*;

import org.nlogo.agent.Observer;
import org.nlogo.agent.World;
import org.nlogo.api.LogoException;
import org.nlogo.api.LogoListBuilder;
import org.nlogo.core.LogoList;
import org.nlogo.headless.HeadlessWorkspace;

public class WolfSheep {
    public static void main(String[] args) {
      java.lang.System.setProperty("netlogo.libraries.disabled", "true");
      try {
        var model = new File("WolfSheep.nlogo");

        String[] initialize = {
          "set number-of-sheep " + args[0],
          "set number-of-wolves " + args[1],
          "set movement-cost " + args[2],
          "set grass-regrowth-rate " + args[3],
          "set energy-gain-from-grass " + args[4],
          "set energy-gain-from-sheep " + args[5],
          "set max-grass " + args[6]
        };

        var sheeps = new ArrayList<Double>();
        var wolves = new ArrayList<Double>();

        var workspace = HeadlessWorkspace.newInstance();
        try {
          workspace.open(model.getPath());
          workspace.command("random-seed " + Integer.parseInt(args[7]));

          for(String cmd: initialize) {
            workspace.command(cmd);
          }

          workspace.command("setup");

          for(int i = 0; i < 400; i++) {
            workspace.command("go");
          }

          for(int i = 0; i < 100; i++) {
            var s = (double) report(workspace, "count sheep");
            sheeps.add(s);
            var w = (double) report(workspace, "count wolves");
            wolves.add(w);
            workspace.command("go");
          }
        } finally {
          workspace.dispose();
        }

        System.out.println(toCSV(sheeps.toArray(new Double[0])));
        System.out.println(toCSV(wolves.toArray(new Double[0])));

      } catch(Exception e) {
        e.printStackTrace();
      }
    }

    public static String toCSV(Double[] v) {
      return Arrays.stream(v).map(String::valueOf).collect(Collectors.joining(","));
    }

    public static double mean(Double[] m) {
      double sum = 0;
      for (int i = 0; i < m.length; i++) {
        sum += m[i];
      }
      return sum / m.length;
    }


  private static LogoList arrayToList(Object[] array){
      LogoListBuilder list = new LogoListBuilder();
      for(Object o: array){
        if(o instanceof Object[]){list.add(arrayToList((Object[]) o));}
        else{list.add(o);}
      }
      return(list.toLogoList());
    }


    private static AbstractCollection listToCollection(LogoList list){
      AbstractCollection collection = new LinkedList();
      for(Object o:list.toJava()){
        if(o instanceof LogoList){collection.add(listToCollection((LogoList) o));}
        else{collection.add(o);}
      }
      return(collection);
    }

    public static void setGlobal(HeadlessWorkspace workspace, String variable, Object value) throws Exception {
      if(value instanceof Object[]){
        workspace.world().setObserverVariableByName(variable,arrayToList((Object[]) value));
      }
      else{
        workspace.world().setObserverVariableByName(variable,value);
      }
    }

    public static Object report(HeadlessWorkspace workspace, String variable) throws Exception {
      Object result = workspace.report(variable);
      if(result instanceof LogoList){
        return listToCollection((LogoList) result);
      } else {
        return result;
      }
    }
} 

