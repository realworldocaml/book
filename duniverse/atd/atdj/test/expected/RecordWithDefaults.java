// Automatically generated; do not edit
package com.mylife.test;
import org.json.*;

public class RecordWithDefaults implements Atdj {
  /**
   * Construct from a fresh record with null fields.
   */
  public RecordWithDefaults() {
  }

  /**
   * Construct from a JSON string.
   */
  public RecordWithDefaults(String s) throws JSONException {
    this(new JSONObject(s));
  }

  RecordWithDefaults(JSONObject jo) throws JSONException {
    if (jo.has("b")) {
      b = jo.optBoolean("b");
    } else {
      b = false;
    }
    if (jo.has("i")) {
      i = jo.optInt("i");
    } else {
      i = 0;
    }
    if (jo.has("s")) {
      s = jo.optString("s");
    } else {
      s = "";
    }
    if (jo.has("o")) {
      o = jo.optBoolean("o");
    } else {
      o = false;
    }
    if (jo.has("l")) {
      l = new java.util.ArrayList<Boolean>();
      for (int _i = 0; _i < jo.optJSONArray("l").length(); ++_i) {
        boolean _tmp = jo.optJSONArray("l").getBoolean(_i);
        l.add(_tmp);
      }
    } else {
      l = new java.util.ArrayList<Boolean>();
    }
    if (jo.has("e")) {
      e = new E(jo.opt("e"));
    }

  }

  public void toJsonBuffer(StringBuilder _out) throws JSONException {
    boolean _isFirst = true;
    _out.append("{");
    if (b != null) {
      if (_isFirst)
        _isFirst = false;
      else
        _out.append(",");
      _out.append("\"b\":");
      _out.append(String.valueOf(b));
    }

    if (i != null) {
      if (_isFirst)
        _isFirst = false;
      else
        _out.append(",");
      _out.append("\"i\":");
      _out.append(String.valueOf(i));
    }

    if (s != null) {
      if (_isFirst)
        _isFirst = false;
      else
        _out.append(",");
      _out.append("\"s\":");
      Util.writeJsonString(_out, s);
    }

    if (o != null) {
      if (_isFirst)
        _isFirst = false;
      else
        _out.append(",");
      _out.append("\"o\":");
      _out.append(String.valueOf(o));
    }

    if (l != null) {
      if (_isFirst)
        _isFirst = false;
      else
        _out.append(",");
      _out.append("\"l\":");
      _out.append("[");
      for (int i = 0; i < l.size(); ++i) {
        _out.append(String.valueOf(l.get(i)));
        if (i < l.size() - 1)
          _out.append(",");
      }
      _out.append("]");
    }

    if (e != null) {
      if (_isFirst)
        _isFirst = false;
      else
        _out.append(",");
      _out.append("\"e\":");
      e.toJsonBuffer(_out);
    }

    _out.append("}");
  }

  public String toJson() throws JSONException {
    StringBuilder out = new StringBuilder(128);
    toJsonBuffer(out);
    return out.toString();
  }
  public Boolean b;
  public Integer i;
  public String s;
  public Boolean o;
  public java.util.ArrayList<Boolean> l;
  public E e;
}
