// Automatically generated; do not edit
package com.mylife.test;
import org.json.*;

/**
 * Construct objects of type b.
 */

public class B {
  Tag t = null;

  public B() {
  }

  public Tag tag() {
    return t;
  }

  /**
   * Define tags for sum type b.
   */
  public enum Tag {
    B
  }

  public B(Object o) throws JSONException {
    String tag = Util.extractTag(o);
    if (tag.equals("B"))
      t = Tag.B;
    else
      throw new JSONException("Invalid tag: " + tag);
  }

  public void setB() {
    /* TODO: clear previously-set field and avoid memory leak */
    t = Tag.B;
  }

  public void toJsonBuffer(StringBuilder _out) throws JSONException {
    if (t == null)
      throw new JSONException("Uninitialized B");
    else {
      switch(t) {
      case B:
        _out.append("\"B\"");
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
