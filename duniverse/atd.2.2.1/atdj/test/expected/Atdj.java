// Automatically generated; do not edit
package com.mylife.test;
import org.json.*;

/**
 * Common utility interface.
 */
public interface Atdj {
  /**
   * Get the JSON string representation, failing if some of the data
   * was not initialized.
   * @return The JSON string.
   */
  String toJson() throws JSONException;

  /**
   * Write the JSON representation to a buffer, failing if some of the data
   * was not initialized.
   */
  void toJsonBuffer(StringBuilder out) throws JSONException;
}
