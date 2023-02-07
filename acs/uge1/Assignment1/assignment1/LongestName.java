package assignment1;

public class LongestName implements Combinator<Employee, String> {
  public void mutate(Employee emp) {
	emp.setName(emp.getName().toLowerCase());
  }

  public String neutral() {
    return "";
  }

  public String combine(String x, String y) { 
    return x.length() > y.length() ? x : y;
  }


  public String get(Employee emp) { 
    return emp.getName();
  }

}
