package com.hx.log.alogrithm.tree.interf;

/**
 * TreeIdExtractor
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/5/2017 9:23 PM
 */
public interface TreeIdExtractor<T, IdType> {

    /**
     * 获取id
     *
     * @return the id of currentObject
     * @author Jerry.X.He
     * @date 5/5/2017 9:24 PM
     * @since 1.0
     */
    IdType id();

    /**
     * 获取parentId
     *
     * @return the parentId of currentObject
     * @author Jerry.X.He
     * @date 5/5/2017 9:24 PM
     * @since 1.0
     */
    IdType parentId();

}
