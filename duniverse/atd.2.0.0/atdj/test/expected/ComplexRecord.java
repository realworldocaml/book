// Automatically generated; do not edit
package com.mylife.test;
import org.json.*;

/**
 * wibble
 */
public class ComplexRecord implements Atdj {
  /**
   * Construct from a fresh record with null fields.
   */
  public ComplexRecord() {
  }

  /**
   * Construct from a JSON string.
   */
  public ComplexRecord(String s) throws JSONException {
    this(new JSONObject(s));
  }

  ComplexRecord(JSONObject jo) throws JSONException {
    b = jo.getBoolean("b");
    i = jo.getInt("i");
    s = jo.getString("s");
    l = new java.util.ArrayList<Boolean>();
    for (int _i = 0; _i < jo.getJSONArray("l").length(); ++_i) {
      boolean _tmp = jo.getJSONArray("l").getBoolean(_i);
      l.add(_tmp);
    }
    sample_sum = new SampleSum(jo.get("sample_sum"));
    if (jo.has("class")) {
      class_ = jo.optInt("class");
    }
    if (jo.has("final")) {
      is_final = jo.optInt("final");
    }
    l2 = new java.util.ArrayList<RecordWithDefaults>();
    for (int _i = 0; _i < jo.getJSONArray("l2").length(); ++_i) {
      JSONObject _tmp = jo.getJSONArray("l2").getJSONObject(_i);
      l2.add(new RecordWithDefaults(_tmp));
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
    else
      throw new JSONException("Uninitialized field b");

    if (i != null) {
      if (_isFirst)
        _isFirst = false;
      else
        _out.append(",");
      _out.append("\"i\":");
      _out.append(String.valueOf(i));
    }
    else
      throw new JSONException("Uninitialized field i");

    if (s != null) {
      if (_isFirst)
        _isFirst = false;
      else
        _out.append(",");
      _out.append("\"s\":");
      Util.writeJsonString(_out, s);
    }
    else
      throw new JSONException("Uninitialized field s");

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
    else
      throw new JSONException("Uninitialized field l");

    if (sample_sum != null) {
      if (_isFirst)
        _isFirst = false;
      else
        _out.append(",");
      _out.append("\"sample_sum\":");
      sample_sum.toJsonBuffer(_out);
    }
    else
      throw new JSONException("Uninitialized field sample_sum");

    if (class_ != null) {
      if (_isFirst)
        _isFirst = false;
      else
        _out.append(",");
      _out.append("\"class\":");
      _out.append(String.valueOf(class_));
    }

    if (is_final != null) {
      if (_isFirst)
        _isFirst = false;
      else
        _out.append(",");
      _out.append("\"final\":");
      _out.append(String.valueOf(is_final));
    }

    if (l2 != null) {
      if (_isFirst)
        _isFirst = false;
      else
        _out.append(",");
      _out.append("\"l2\":");
      _out.append("[");
      for (int i = 0; i < l2.size(); ++i) {
        l2.get(i).toJsonBuffer(_out);
        if (i < l2.size() - 1)
          _out.append(",");
      }
      _out.append("]");
    }
    else
      throw new JSONException("Uninitialized field l2");

    _out.append("}");
  }

  public String toJson() throws JSONException {
    StringBuilder out = new StringBuilder(128);
    toJsonBuffer(out);
    return out.toString();
  }
  /**
   * foo bar baz
   */
  public Boolean b;
  public Integer i;
  public String s;
  public java.util.ArrayList<Boolean> l;
  public SampleSum sample_sum;
  public Integer class_;
  public Integer is_final;
  public java.util.ArrayList<RecordWithDefaults> l2;
}
