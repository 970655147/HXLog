package com.hx.log.idx.idx_filter;

import com.hx.common.interf.idx.IdxFilter;

/**
 * ������������� <[=] min, ������˵�
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 10:15 PM
 */
public class LowerBoundsIdxFilter implements IdxFilter {

    /**
     * ����
     */
    private int min;
    /**
     * �Ƿ��������
     */
    private boolean containsEq;

    /**
     * ��ʼ��
     *
     * @param min        ����
     * @param containsEq �Ƿ��������
     * @since 1.0
     */
    public LowerBoundsIdxFilter(int min, boolean containsEq) {
        this.min = min;
        this.containsEq = containsEq;
    }

    @Override
    public boolean filter(int idx) {
        return containsEq ? (idx >= min) : (idx > min);
    }

    @Override
    public IdxFilter copy() {
        return new LowerBoundsIdxFilter(min, containsEq);
    }
}
