// Automatically generated; do not edit
package com.mylife.test;
import org.json.*;

/**
 * Construct objects of type a.
 */

public class A {
  Tag t = null;

  public A() {
  }

  public Tag tag() {
    return t;
  }

  /**
   * Define tags for sum type a.
   */
  public enum Tag {
    A
  }

  public A(Object o) throws JSONException {
    String tag = Util.extractTag(o);
    if (tag.equals("A")) {
      field_a = new java.util.ArrayList<B>();
      for (int _i = 0; _i < ((JSONArray)o).getJSONArray(1).length(); ++_i) {
        Object _tmp = ((JSONArray)o).getJSONArray(1).get(_i);
        field_a.add(new B(_tmp));
      }

      t = Tag.A;
    }
    else
      throw new JSONException("Invalid tag: " + tag);
  }

  java.util.ArrayList<B> field_a = null;
  public void setA(java.util.ArrayList<B> x) {
    /* TODO: clear previously-set field in order to avoid memory leak */
    t = Tag.A;
    field_a = x;
  }
  public java.util.ArrayList<B> getA() {
    if (t == Tag.A)
      return field_a;
    else
      return null;
  }

  public void toJsonBuffer(StringBuilder _out) throws JSONException {
    if (t == null)
      throw new JSONException("Uninitialized A");
    else {
      switch(t) {
      case A:
         _out.append("[\"A\",");
         _out.append("[");
         for (int i = 0; i < field_a.size(); ++i) {
           field_a.get(i).toJsonBuffer(_out);
           if (i < field_a.size() - 1)
             _out.append(",");
         }
         _out.append("]");
         _out.append("]");
         break;
      default:
        break; /* unused; keeps compiler happy */
      }
    }
  }

  public String toJson() throws JSONException {
    StringBuilder out = new StringBuilder(128);
    toJsonBuffer(out);
    return out.toString();
  }
}
