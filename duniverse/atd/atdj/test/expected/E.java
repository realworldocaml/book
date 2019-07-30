// Automatically generated; do not edit
package com.mylife.test;
import org.json.*;

/**
 * Construct objects of type e.
 */

public class E {
  Tag t = null;

  public E() {
  }

  public Tag tag() {
    return t;
  }

  /**
   * Define tags for sum type e.
   */
  public enum Tag {
    ALPHA, BETA
  }

  public E(Object o) throws JSONException {
    String tag = Util.extractTag(o);
    if (tag.equals("Alpha"))
      t = Tag.ALPHA;
    else if (tag.equals("Beta"))
      t = Tag.BETA;
    else
      throw new JSONException("Invalid tag: " + tag);
  }

  public void setAlpha() {
    /* TODO: clear previously-set field and avoid memory leak */
    t = Tag.ALPHA;
  }

  public void setBeta() {
    /* TODO: clear previously-set field and avoid memory leak */
    t = Tag.BETA;
  }

  public void toJsonBuffer(StringBuilder _out) throws JSONException {
    if (t == null)
      throw new JSONException("Uninitialized E");
    else {
      switch(t) {
      case ALPHA:
        _out.append("\"Alpha\"");
        break;
      case BETA:
        _out.append("\"Beta\"");
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
