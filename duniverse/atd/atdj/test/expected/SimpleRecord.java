// Automatically generated; do not edit
package com.mylife.test;
import org.json.*;

public class SimpleRecord implements Atdj {
  /**
   * Construct from a fresh record with null fields.
   */
  public SimpleRecord() {
  }

  /**
   * Construct from a JSON string.
   */
  public SimpleRecord(String s) throws JSONException {
    this(new JSONObject(s));
  }

  SimpleRecord(JSONObject jo) throws JSONException {
    if (jo.has("o")) {
      o = jo.optBoolean("o");
    }

  }

  public void toJsonBuffer(StringBuilder _out) throws JSONException {
    boolean _isFirst = true;
    _out.append("{");
    if (o != null) {
      if (_isFirst)
        _isFirst = false;
      else
        _out.append(",");
      _out.append("\"o\":");
      _out.append(String.valueOf(o));
    }

    _out.append("}");
  }

  public String toJson() throws JSONException {
    StringBuilder out = new StringBuilder(128);
    toJsonBuffer(out);
    return out.toString();
  }
  public Boolean o;
}
