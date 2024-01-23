import java.util.*;
import java.io.File;

import org.nlogo.agent.Observer;
import org.nlogo.agent.World;
import org.nlogo.api.LogoException;
import org.nlogo.api.LogoListBuilder;
import org.nlogo.core.LogoList;
import org.nlogo.headless.HeadlessWorkspace;

public class Traffic {
    public static void main(String[] args) {
      try {
        var model = new File("Traffic_2_Lanes.nlogo");

        String[] initialize = {
          "set number-of-cars " + args[0],
          "set acceleration " + args[1],
          "set deceleration " + args[2],
          "set max-patience " + args[3]
        };

        var speeds = new ArrayList<Double>();
        var patiences = new ArrayList<Double>();

        var workspace = HeadlessWorkspace.newInstance();
        try {
          workspace.open(model.getPath());
          workspace.command("random-seed " + Integer.parseInt(args[4]));
          workspace.command("setup");

          for(String cmd: initialize) {
            workspace.command(cmd);
          }

          for(int i = 0; i < 100; i++) {
            workspace.command("go");
          }

          for(int i = 0; i < 100; i++) {
            var speed = (double) report(workspace, "mean [ speed ] of turtles");
            speeds.add(speed);
            var patience = (double) report(workspace, "mean [ patience ] of turtles");
            patiences.add(patience);
            workspace.command("go");
          }
        } finally {
          workspace.dispose();
        }

        System.out.println(mean(speeds.toArray(new Double[0])));
        System.out.println(mean(patiences.toArray(new Double[0])));

      } catch(Exception e) {
        e.printStackTrace();
      }
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

