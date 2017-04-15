package com.hx.log.json.interf;

import net.sf.json.JSONException;

/**
 * JSON
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/15/2017 11:45 AM
 */
public interface JSON {

    /**
     * ��ȡ��ǰJSON��type
     *
     * @return true if currentStartIdx JSON is instance of JSONArray
     * @author Jerry.X.He
     * @date 4/15/2017 11:46 AM
     * @since 1.0
     */
    JSONType type();

    /**
     * ��ȡ��ǰJSON��װ��Object
     *
     * @return the object that current JSON wrapped
     * @author Jerry.X.He
     * @date 4/15/2017 11:46 AM
     * @since 1.0
     */
    Object value();

    /**
     * ��ȡ��ǰJSON�Ƿ�Ϊ��
     *
     * @return true if there are no element in currentStartIdx JSON
     * @author Jerry.X.He
     * @date 4/15/2017 11:46 AM
     * @since 1.0
     */
    boolean isEmpty();

    /**
     * ��ȡ��ǰJSON��Ԫ�ص�����
     *
     * @return the element num of current JSON
     * @author Jerry.X.He
     * @date 4/15/2017 11:46 AM
     * @since 1.0
     */
    int size();

    /**
     * Make a prettyprinted JSON text.
     * <p>
     * Warning: This method assumes that the data structure is acyclical.
     *
     * @param indentFactor The number of spaces to add to each level of
     *        indentation.
     * @return a printable, displayable, portable, transmittable representation
     *         of the object, beginning with <code>{</code>&nbsp;<small>(left
     *         brace)</small> and ending with <code>}</code>&nbsp;<small>(right
     *         brace)</small>.
     * @author Jerry.X.He
     * @date 4/15/2017 11:46 AM
     * @since 1.0
     */
    String toString( int indentFactor );

}
