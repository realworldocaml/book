// Automatically generated; do not edit
package com.mylife.test;
import org.json.*;

/**
 * Construct objects of type sample_sum.
 */

public class SampleSum {
  Tag t = null;

  public SampleSum() {
  }

  public Tag tag() {
    return t;
  }

  /**
   * Define tags for sum type sample_sum.
   */
  public enum Tag {
    SIMPLE_TAG, BOOL, INT_, FLOAT_, S, SIMPLE_RECORD, COMPLEX_RECORD, RECORD_WITH_DEFAULTS
  }

  public SampleSum(Object o) throws JSONException {
    String tag = Util.extractTag(o);
    if (tag.equals("Simple_tag"))
      t = Tag.SIMPLE_TAG;
    else if (tag.equals("Bool")) {
      field_bool = ((JSONArray)o).getBoolean(1);

      t = Tag.BOOL;
    }
    else if (tag.equals("Int")) {
      field_int_ = ((JSONArray)o).getInt(1);

      t = Tag.INT_;
    }
    else if (tag.equals("Float")) {
      field_float_ = ((JSONArray)o).getDouble(1);

      t = Tag.FLOAT_;
    }
    else if (tag.equals("String")) {
      field_s = ((JSONArray)o).getString(1);

      t = Tag.S;
    }
    else if (tag.equals("Simple_record")) {
      field_simple_record = new SimpleRecord(((JSONArray)o).getJSONObject(1));

      t = Tag.SIMPLE_RECORD;
    }
    else if (tag.equals("Complex_record")) {
      field_complex_record = new ComplexRecord(((JSONArray)o).getJSONObject(1));

      t = Tag.COMPLEX_RECORD;
    }
    else if (tag.equals("Record_with_defaults")) {
      field_record_with_defaults = new RecordWithDefaults(((JSONArray)o).getJSONObject(1));

      t = Tag.RECORD_WITH_DEFAULTS;
    }
    else
      throw new JSONException("Invalid tag: " + tag);
  }

  public void setSimpleTag() {
    /* TODO: clear previously-set field and avoid memory leak */
    t = Tag.SIMPLE_TAG;
  }

  Boolean field_bool = null;
  public void setBool(Boolean x) {
    /* TODO: clear previously-set field in order to avoid memory leak */
    t = Tag.BOOL;
    field_bool = x;
  }
  public Boolean getBool() {
    if (t == Tag.BOOL)
      return field_bool;
    else
      return null;
  }

  Integer field_int_ = null;
  public void setInt(Integer x) {
    /* TODO: clear previously-set field in order to avoid memory leak */
    t = Tag.INT_;
    field_int_ = x;
  }
  public Integer getInt() {
    if (t == Tag.INT_)
      return field_int_;
    else
      return null;
  }

  Double field_float_ = null;
  public void setFloat(Double x) {
    /* TODO: clear previously-set field in order to avoid memory leak */
    t = Tag.FLOAT_;
    field_float_ = x;
  }
  public Double getFloat() {
    if (t == Tag.FLOAT_)
      return field_float_;
    else
      return null;
  }

  String field_s = null;
  public void setS(String x) {
    /* TODO: clear previously-set field in order to avoid memory leak */
    t = Tag.S;
    field_s = x;
  }
  public String getS() {
    if (t == Tag.S)
      return field_s;
    else
      return null;
  }

  SimpleRecord field_simple_record = null;
  public void setSimpleRecord(SimpleRecord x) {
    /* TODO: clear previously-set field in order to avoid memory leak */
    t = Tag.SIMPLE_RECORD;
    field_simple_record = x;
  }
  public SimpleRecord getSimpleRecord() {
    if (t == Tag.SIMPLE_RECORD)
      return field_simple_record;
    else
      return null;
  }

  ComplexRecord field_complex_record = null;
  public void setComplexRecord(ComplexRecord x) {
    /* TODO: clear previously-set field in order to avoid memory leak */
    t = Tag.COMPLEX_RECORD;
    field_complex_record = x;
  }
  public ComplexRecord getComplexRecord() {
    if (t == Tag.COMPLEX_RECORD)
      return field_complex_record;
    else
      return null;
  }

  RecordWithDefaults field_record_with_defaults = null;
  public void setRecordWithDefaults(RecordWithDefaults x) {
    /* TODO: clear previously-set field in order to avoid memory leak */
    t = Tag.RECORD_WITH_DEFAULTS;
    field_record_with_defaults = x;
  }
  public RecordWithDefaults getRecordWithDefaults() {
    if (t == Tag.RECORD_WITH_DEFAULTS)
      return field_record_with_defaults;
    else
      return null;
  }

  public void toJsonBuffer(StringBuilder _out) throws JSONException {
    if (t == null)
      throw new JSONException("Uninitialized SampleSum");
    else {
      switch(t) {
      case SIMPLE_TAG:
        _out.append("\"Simple_tag\"");
        break;
      case BOOL:
         _out.append("[\"Bool\",");
         _out.append(String.valueOf(field_bool));
         _out.append("]");
         break;
      case INT_:
         _out.append("[\"Int\",");
         _out.append(String.valueOf(field_int_));
         _out.append("]");
         break;
      case FLOAT_:
         _out.append("[\"Float\",");
         _out.append(String.valueOf(field_float_));
         _out.append("]");
         break;
      case S:
         _out.append("[\"String\",");
         Util.writeJsonString(_out, field_s);
         _out.append("]");
         break;
      case SIMPLE_RECORD:
         _out.append("[\"Simple_record\",");
         field_simple_record.toJsonBuffer(_out);
         _out.append("]");
         break;
      case COMPLEX_RECORD:
         _out.append("[\"Complex_record\",");
         field_complex_record.toJsonBuffer(_out);
         _out.append("]");
         break;
      case RECORD_WITH_DEFAULTS:
         _out.append("[\"Record_with_defaults\",");
         field_record_with_defaults.toJsonBuffer(_out);
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
